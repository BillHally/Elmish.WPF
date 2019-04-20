namespace Elmish.WPF.Internal

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.ComponentModel
open System.Windows
open Elmish.WPF
open System.Reflection
open System.Reflection.Emit

type CustomPropertyDescriptor(name : string, componentType : Type, propertyType : Type, getValue, isReadOnly, setValue) =
  inherit PropertyDescriptor(name, [||])

  override __.ComponentType = componentType
  override __.PropertyType  = propertyType
  override __.IsReadOnly    = isReadOnly

  override __.CanResetValue(x) = false
  override __.ResetValue(x : obj) = failwithf "ResetValue shouldn't get invoked (parameter: %A)" x

  override __.GetValue(x : obj) = getValue x
  override __.SetValue(x, v) = setValue x v

  override __.ShouldSerializeValue(x) = true

/// Represents all necessary data used in an active binding.
type Binding<'model, 'msg> =
  | OneWay of propertyType: Type * get: ('model -> obj)
  | OneWayLazy of
      propertyType: Type
      * currentVal: Lazy<obj> ref
      * get: ('model -> obj)
      * map: (obj -> obj)
      * equals: (obj -> obj -> bool)
  | OneWaySeq of
      itemType: Type
      * vals: ObservableCollection<obj>
      * get: ('model -> obj)
      * map: (obj -> obj seq)
      * equals: (obj -> obj -> bool)
      * getId: (obj -> obj)
      * itemEquals: (obj -> obj -> bool)
  | TwoWay of
      propertyType: Type
      * get: ('model -> obj)
      * set: (obj -> 'model -> 'msg)
  | TwoWayValidate of
      propertyType: Type
      * get: ('model -> obj)
      * set: (obj -> 'model -> 'msg)
      * validate: ('model -> Result<obj, string>)
  | TwoWayIfValid of
      propertyType: Type
      * get: ('model -> obj)
      * set: (obj -> 'model -> Result<'msg, string>)
  | Cmd of cmd: Command * canExec: ('model -> bool)
  | CmdIfValid of cmd: Command * exec: ('model -> Result<'msg, obj>)
  | ParamCmd of cmd: Command
  | SubModel of
      model: ViewModel<obj, obj> option ref
      * getModel: ('model -> obj option)
      * getBindings: (unit -> BindingSpec<obj, obj> list)
      * toMsg: (obj -> 'msg)
  | SubModelSeq of
      vms: ObservableCollection<obj> //<ViewModel<obj, obj>>
      * getModels: ('model -> obj seq)
      * getId: (obj -> obj)
      * getBindings: (unit -> BindingSpec<obj, obj> list)
      * toMsg: (obj * obj -> 'msg)

and [<AllowNullLiteral>] ViewModel<'model, 'msg>
      ( initialModel: 'model,
        dispatch: 'msg -> unit,
        bindingSpecs: BindingSpec<'model, 'msg> list,
        config: ElmConfig)
      as this =

  let log fmt =
    let innerLog (str: string) =
      if config.LogConsole then Console.WriteLine(str)
      if config.LogTrace then Diagnostics.Trace.WriteLine(str)
    Printf.kprintf innerLog fmt

  let mutable currentModel = initialModel

  let propertyChanged = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
  let errorsChanged = DelegateEvent<EventHandler<DataErrorsChangedEventArgs>>()

  /// Error messages keyed by property name.
  let errors = Dictionary<string, string>()

  let notifyPropertyChanged propName =
    log "[VM] Triggering PropertyChanged for binding %s" propName
    propertyChanged.Trigger(this, PropertyChangedEventArgs propName)

  let raiseCanExecuteChanged (cmd: Command) =
    cmd.RaiseCanExecuteChanged ()

  let setError error propName =
    match errors.TryGetValue propName with
    | true, err when err = error -> ()
    | _ ->
        log "[VM] Setting error for binding %s to \"%s\"" propName error
        errors.[propName] <- error
        errorsChanged.Trigger([| box this; box <| DataErrorsChangedEventArgs propName |])

  let removeError propName =
    if errors.Remove propName then
      log "[VM] Removing error for binding %s" propName
      errorsChanged.Trigger([| box this; box <| DataErrorsChangedEventArgs propName |])

  let initializeBinding bindingSpec =
    match bindingSpec with
    | OneWaySpec (t, get) -> OneWay (t, get)
    | OneWayLazySpec (t, get, map, equals) ->
        OneWayLazy (t, ref <| lazy (initialModel |> get |> map), get, map, equals)
    | OneWaySeqLazySpec (t, get, map, equals, getId, itemEquals) ->
        let vals = ObservableCollection<obj>(initialModel |> get |> map)
        OneWaySeq (t, vals, get, map, equals, getId, itemEquals)
    | TwoWaySpec (t, get, set) -> TwoWay (t, get, set)
    | TwoWayValidateSpec (t, get, set, validate) -> TwoWayValidate (t, get, set, validate)
    | TwoWayIfValidSpec (t, get, set) -> TwoWayIfValid (t, get, set)
    | CmdSpec (exec, canExec) ->
        let execute _ = exec currentModel |> dispatch
        let canExecute _ = canExec currentModel
        ParamCmd <| Command(execute, canExecute, false)
    | CmdIfValidSpec exec ->
        let execute _ = exec currentModel |> Result.iter dispatch
        let canExecute _ = exec currentModel |> Result.isOk
        CmdIfValid (Command(execute, canExecute, false), exec)
    | ParamCmdSpec (exec, canExec, autoRequery) ->
        let execute param = dispatch <| exec param currentModel
        let canExecute param = canExec param currentModel
        ParamCmd <| Command(execute, canExecute, autoRequery)
    | SubModelSpec (getModel, getBindings, toMsg) ->
        match getModel initialModel with
        | None -> SubModel (ref None, getModel, getBindings, toMsg)
        | Some m ->
            let vm = ViewModel.Create(getBindings (), config) (m, toMsg >> dispatch)
            SubModel (ref <| Some vm, getModel, getBindings, toMsg)
    | SubModelSeqSpec (getModels, getId, getBindings, toMsg) ->
        let vms =
          let create = ViewModel.Create(getBindings (), config)
          getModels initialModel
          |> Seq.map (fun m -> create (m, (fun msg -> toMsg (getId m, msg) |> dispatch)) :> obj)
          |> ObservableCollection
        SubModelSeq (vms, getModels, getId, getBindings, toMsg)

  let setInitialError name = function
    | TwoWayValidate (_, _, _, validate) ->
        match validate initialModel with
        | Ok _ -> ()
        | Error error -> setError error name
    | TwoWayIfValid (_, get, set) ->
        let initialValue = get initialModel
        match set initialValue initialModel with
        | Ok _ -> ()
        | Error error -> setError error name
    | _ -> ()

  let bindings =
    let dict = Dictionary<string, Binding<'model, 'msg>>()
    for spec in bindingSpecs do
      log "[VM] Initializing binding %s" spec.Name
      let binding = initializeBinding spec.Data
      dict.Add(spec.Name, binding)
      setInitialError spec.Name binding
    dict

  /// Updates the binding value (for relevant bindings) and returns a value
  /// indicating whether to trigger PropertyChanged for this binding
  let updateValue newModel binding =
    match binding with
    | OneWay (_, get)
    | TwoWay (_, get, _)
    | TwoWayValidate (_, get, _, _)
    | TwoWayIfValid (_, get, _) ->
        get currentModel <> get newModel
    | OneWayLazy (_, currentVal, get, map, equals) ->
        if equals (get newModel) (get currentModel) then false
        else
          currentVal := lazy (newModel |> get |> map)
          true
    | OneWaySeq (_, vals, get', map, equals', getId, itemEquals) ->
        if not <| equals' (get' newModel) (get' currentModel) then
          let newVals = newModel |> get' |> map
          // Prune and update existing values
          let newLookup = Dictionary<_,_>()
          for v in newVals do newLookup.Add(getId v, v)
          for existingVal in vals |> Seq.toList do
            match newLookup.TryGetValue (getId existingVal) with
            | false, _ -> vals.Remove(existingVal) |> ignore
            | true, newVal when not (itemEquals newVal existingVal) ->
                vals.Remove existingVal |> ignore
                vals.Add newVal  // Will be sorted later
            | _ -> ()
          // Add new values that don't currently exist
          let valuesToAdd =
            newVals
            |> Seq.filter (fun m ->
                  vals |> Seq.exists (fun existingVal -> getId m = getId existingVal) |> not
            )
          for m in valuesToAdd do vals.Add m
          // Reorder according to new model list
          for newIdx, newVal in newVals |> Seq.indexed do
            let oldIdx =
              vals
              |> Seq.indexed
              |> Seq.find (fun (_, existingVal) -> getId existingVal = getId newVal)
              |> fst
            if oldIdx <> newIdx then vals.Move(oldIdx, newIdx)
        false
    | Cmd _
    | CmdIfValid _
    | ParamCmd _ ->
        false
    | SubModel ((vm: ViewModel<obj, obj> option ref), (getModel: 'model -> obj option), getBindings, toMsg) ->
        match !vm, getModel newModel with
        | None, None -> false
        | Some _, None ->
            vm := None
            true
        | None, Some m ->
            vm := Some <| ViewModel.Create(getBindings (), config) (m, toMsg >> dispatch)
            true
        | Some vm, Some m ->
            vm.UpdateModel(m)
            false
    | SubModelSeq (vms, getModels, getId, getBindings, toMsg) ->
        let newSubModels = getModels newModel
        // Prune and update existing models
        let newLookup = Dictionary<_,_>()
        for m in newSubModels do newLookup.Add(getId m, m)
        for vm in vms |> Seq.cast<ViewModel<obj, obj>> |> Seq.toList do
          match newLookup.TryGetValue (getId vm.CurrentModel) with
          | false, _ -> vms.Remove(vm) |> ignore
          | true, newSubModel -> vm.UpdateModel newSubModel
        // Add new models that don't currently exist
        let modelsToAdd =
          newSubModels
          |> Seq.filter (fun m ->
                vms |> Seq.cast<ViewModel<obj, obj>> |> Seq.exists (fun vm -> getId m = getId vm.CurrentModel) |> not
          )
        for m in modelsToAdd do
          vms.Add <| ViewModel.Create(getBindings (), config) (m, (fun msg -> toMsg (getId m, msg) |> dispatch))
        // Reorder according to new model list
        for newIdx, newSubModel in newSubModels |> Seq.indexed do
          let oldIdx =
            vms
            |> Seq.cast<ViewModel<obj, obj>> 
            |> Seq.indexed
            |> Seq.find (fun (_, vm) -> getId newSubModel = getId vm.CurrentModel)
            |> fst
          if oldIdx <> newIdx then vms.Move(oldIdx, newIdx)
        false

  /// Returns the command associated with a command binding if the command's
  /// CanExecuteChanged should be triggered.
  let getCmdIfCanExecChanged newModel binding =
    match binding with
    | OneWay _
    | OneWayLazy _
    | OneWaySeq _
    | TwoWay _
    | TwoWayValidate _
    | TwoWayIfValid _
    | SubModel _
    | SubModelSeq _ ->
        None
    | Cmd (cmd, canExec) ->
        if canExec newModel = canExec currentModel then None else Some cmd
    | CmdIfValid (cmd, exec) ->
        match exec currentModel, exec newModel with
        | Ok _, Error _ | Error _, Ok _ -> Some cmd
        | _ -> None
    | ParamCmd cmd -> Some cmd

  /// Updates the validation status for a binding.
  let updateValidationStatus name binding =
    match binding with
    | TwoWayValidate (_, _, _, validate) ->
        match validate currentModel with
        | Ok _ -> removeError name
        | Error err -> setError err name
    | _ -> ()

  member private __.CurrentModel : 'model = currentModel

  member __.UpdateModel (newModel: 'model) : unit =
    log "[VM] UpdateModel %s" <| newModel.GetType().FullName
    let propsToNotify =
      bindings
      |> Seq.toList
      |> List.filter (Kvp.value >> updateValue newModel)
      |> List.map Kvp.key
    let cmdsToNotify =
      bindings
      |> Seq.toList
      |> List.choose (Kvp.value >> getCmdIfCanExecChanged newModel)
    currentModel <- newModel
    propsToNotify |> List.iter notifyPropertyChanged
    cmdsToNotify |> List.iter raiseCanExecuteChanged
    for Kvp (name, binding) in bindings do
      updateValidationStatus name binding

  member __.TryGetMember memberName : 'property =
    log "[VM] TryGetMember %s" memberName
    match bindings.TryGetValue memberName with
    | false, _ ->
        log "[VM] TryGetMember FAILED: Property %s doesn't exist" memberName
        Unchecked.defaultof<'property>
    | true, binding ->
        match binding with
        | OneWay (_, get)
        | TwoWay (_, get, _)
        | TwoWayValidate (_, get, _, _)
        | TwoWayIfValid (_, get, _) ->
            get currentModel
        | OneWayLazy (_, value, _, _, _) ->
            (!value).Value
        | OneWaySeq (_, vals, _, _, _, _, _) ->
            box vals
        | Cmd (cmd, _)
        | CmdIfValid (cmd, _)
        | ParamCmd cmd ->
            box cmd
        | SubModel (vm, _, _, _) -> !vm |> Option.toObj :> obj
        | SubModelSeq (vms, _, _, _, _) -> box vms
        :?> 'property
    |> (fun v -> log "[VM] TryGetMember %s returning: %A" memberName v; v)

  member __.TrySetMember (memberName, value : 'property) =
    try
      log "[VM] TrySetMember %s: %A" memberName value
      match bindings.TryGetValue memberName with
      | false, _ ->
          log "[VM] TrySetMember FAILED: Property %s doesn't exist" memberName
      | true, binding ->
          log "[VM] TrySetMember %s: %A: %A" memberName value binding
          match binding with
          | TwoWay (_, _, set)
          | TwoWayValidate (_, _, set, _) ->
              dispatch <| set value currentModel
          | TwoWayIfValid (_, _, set) ->
              match set value currentModel with
              | Ok msg ->
                  removeError memberName
                  dispatch msg
              | Error err -> setError err memberName
          | OneWay _
          | OneWayLazy _
          | OneWaySeq _
          | Cmd _
          | CmdIfValid _
          | ParamCmd _
          | SubModel _
          | SubModelSeq _ ->
              log "[VM] TrySetMember FAILED: Binding %s is read-only" memberName
    with
    | ex ->
      log "[VM]: TrySetMember FAILED: %A" ex

  interface INotifyPropertyChanged with
    [<CLIEvent>]
    member __.PropertyChanged = propertyChanged.Publish

  interface INotifyDataErrorInfo with
    [<CLIEvent>]
    member __.ErrorsChanged = errorsChanged.Publish
    member __.HasErrors =
      errors.Count > 0
    member __.GetErrors propName =
      log "[VM] GetErrors %s" (propName |> Option.ofObj |> Option.defaultValue "<null>")
      match errors.TryGetValue propName with
      | true, err -> upcast [err]
      | false, _ -> upcast []

and ViewModel private () =
  static let mutable count = -1

  static member private GetProperties (bindings : BindingSpec<'model, 'msg> list) =
    seq {
      for b in bindings do

        let name = b.Name

        let propertyType =
          match b.Data with
          | OneWaySpec         (t, _)
          | OneWayLazySpec     (t, _, _, _)
          | OneWaySeqLazySpec  (t, _, _, _, _, _)
          | TwoWaySpec         (t, _, _)   
          | TwoWayValidateSpec (t, _, _, _)
          | TwoWayIfValidSpec  (t, _, _) -> t
          | CmdSpec        (_, _)
          | CmdIfValidSpec (_)
          | ParamCmdSpec    _ -> typeof<Command>
          | SubModelSpec    (_, _, _) -> typeof<ViewModel<obj, obj>>
          | SubModelSeqSpec (_, _, _, _) -> typeof<ObservableCollection<obj>> //ViewModel<obj, obj>>>

        yield name, propertyType
      }

  static member Create<'model, 'msg>(bindingSpecs: BindingSpec<'model, 'msg> list, config: ElmConfig)
      : 'model * ('msg -> unit) -> ViewModel<'model, 'msg> =
    count <- count + 1
    let derivedTypeName = sprintf "ViewModel_%s_%s_%d" typeof<'model>.Name typeof<'msg>.Name count

    let properties : seq<string * Type> = ViewModel.GetProperties bindingSpecs

    let baseType = typeof<ViewModel<'model, 'msg>>
    let baseTypeCtor = baseType.GetConstructors() |> Seq.exactlyOne

    let tryGetMember = baseType.GetMethod("TryGetMember")
    let trySetMember = baseType.GetMethod("TrySetMember")

    let assemblyName    = AssemblyName(sprintf "%s_Assembly" derivedTypeName)
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)

    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, emitSymbolInfo = true, fileName = sprintf "%s.dll" assemblyName.Name)
    let typeBuilder   = moduleBuilder.DefineType(derivedTypeName, TypeAttributes.Public, baseType)

    let ctor =
      typeBuilder.DefineConstructor(
        MethodAttributes.Public,
        CallingConventions.Standard,
        baseTypeCtor.GetParameters() |> Array.map (fun p -> p.ParameterType))

    let ctorIL = ctor.GetILGenerator()
    ctorIL.Emit(OpCodes.Ldarg_0) // Instance being initialized
    ctorIL.Emit(OpCodes.Ldarg_1) // initialModel
    ctorIL.Emit(OpCodes.Ldarg_2) // dispatch
    ctorIL.Emit(OpCodes.Ldarg_3) // bindingSpecs
    ctorIL.Emit(OpCodes.Ldarg_S, 4uy) // config
    ctorIL.Emit(OpCodes.Call, baseTypeCtor)
    ctorIL.Emit(OpCodes.Ret)

    let writeLineInGetter = typeof<Console>.GetMethod("WriteLine", [| typeof<string> |])
    let writeLineInSetter = typeof<Console>.GetMethod("WriteLine", [| typeof<string>; typeof<obj> |])

    for (propertyName, propertyType) in properties do
        let propertyBuilder = typeBuilder.DefineProperty(propertyName, PropertyAttributes.HasDefault, propertyType, Type.EmptyTypes)

        let getSetAttr = MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig

        ///////////////////////////////
        // getter
        let getMethod = typeBuilder.DefineMethod("get_" + propertyName, getSetAttr, propertyType, Type.EmptyTypes)
        let getPropertyIL = getMethod.GetILGenerator()

        ////
        // Write the getter's name to the console
        getPropertyIL.Emit(OpCodes.Ldstr, getMethod.Name)
        getPropertyIL.Emit(OpCodes.Call, writeLineInGetter)
        ////

        getPropertyIL.Emit(OpCodes.Ldarg_0) // instance
        getPropertyIL.Emit(OpCodes.Ldstr, propertyName)
        getPropertyIL.Emit(OpCodes.Call, tryGetMember.MakeGenericMethod propertyType)
        getPropertyIL.Emit(OpCodes.Ret)
        propertyBuilder.SetGetMethod(getMethod)

        ///////////////////////////////
        // setter
        let setMethod = typeBuilder.DefineMethod("set_" + propertyName, getSetAttr, typeof<Void>, [| propertyType |])
        let setPropertyIL = setMethod.GetILGenerator()

        ////
        // Write the setter's name and argument to the console
        setPropertyIL.Emit(OpCodes.Ldstr, sprintf "%s <{0}>" setMethod.Name)
        setPropertyIL.Emit(OpCodes.Ldarg_1) // value
        if propertyType.IsValueType then setPropertyIL.Emit(OpCodes.Box, propertyType)
        setPropertyIL.Emit(OpCodes.Call, writeLineInSetter)
        ////

        ////
        // Invoke TrySetMember
        setPropertyIL.Emit(OpCodes.Ldarg_0) // instance
        setPropertyIL.Emit(OpCodes.Ldstr, propertyName)
        setPropertyIL.Emit(OpCodes.Ldarg_1) // value
        setPropertyIL.Emit(OpCodes.Call, trySetMember.MakeGenericMethod propertyType)
        ////

        setPropertyIL.Emit(OpCodes.Ret)
        propertyBuilder.SetSetMethod(setMethod)

    let t = typeBuilder.CreateType()
    assemblyBuilder.Save(sprintf "%s.dll" assemblyName.Name)
    //////////////////
    let ctor = t.GetConstructors() |> Seq.exactlyOne

    fun (initialModel, dispatch) ->
      let instance = ctor.Invoke([| initialModel; dispatch; bindingSpecs; config |])
      instance :?> ViewModel<'model, 'msg>
