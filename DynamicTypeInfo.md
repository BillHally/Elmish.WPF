# Providing dynamic type information to the WPF DataGrid

## System.ComponentModel's support for dynamic type information

The namespace [System.ComponentModel](https://docs.microsoft.com/en-us/dotnet/api/system.componentmodel?view=netstandard-2.0)
provides support for dynamically extending type information at runtime. This
support is in turn used by various types.

### Types

| Namespace | Type name | Description
|--|--|--
| System.Component.Model | [CustomTypeDescriptor](https://docs.microsoft.com/en-us/dotnet/api/system.componentmodel.customtypedescriptor?view=netstandard-2.0) | Provides a simple default implementation of the `ICustomTypeDescriptor` interface
| System.Component.Model | [ICustomTypeDescriptor](https://docs.microsoft.com/en-us/dotnet/api/system.componentmodel.icustomtypedescriptor?view=netstandard-2.0) | Provides an interface that supplies dynamic custom type information for an object
| System.Component.Model | [IItemProperties](https://docs.microsoft.com/en-us/dotnet/api/system.componentmodel.iitemproperties?view=netframework-4.7.2) | Defines a property that provides information about an object's properties
| System.Component.Model | [PropertyDescriptor](https://docs.microsoft.com/en-us/dotnet/api/system.componentmodel.propertydescriptor?view=netstandard-2.0) | Provides an abstraction of a property on a class
| System.Component.Model | [TypeDescriptor](https://docs.microsoft.com/en-us/dotnet/api/system.componentmodel.typedescriptor?view=netstandard-2.0) | Provides information about the characteristics for a component, such as its attributes, properties, and events

## Use by DataGrid

The WPF DataGrid uses this functionality to provide automatic generation of columns.

| Namespace | Type | Description
| --|--|--
| System.Windows.Controls | [DataGrid](https://docs.microsoft.com/en-us/dotnet/api/system.windows.controls.datagrid?view=netframework-4.7.2) | The WPF datagrid type
| System.Windows.Controls | [CollectionView](https://docs.microsoft.com/en-us/dotnet/api/system.windows.data.collectionview?view=netframework-4.7.2) | Represents a view for grouping, sorting, filtering, and navigating a data collection
| System.Windows.Controls | [ItemCollection](https://docs.microsoft.com/en-us/dotnet/api/system.windows.controls.itemcollection?view=netframework-4.7.2) | The type of the property [DataGrid.Items]() (implements `IItemProperties`)

### How it works

1. `DataGrid`'s `AddAutoColumns` member [casts it's Items property](https://referencesource.microsoft.com/#PresentationFramework/src/Framework/System/Windows/Controls/DataGrid.cs,7806)
   to `IItemProperties`
2. The `Items` property (which is an instance of `ItemCollection`)
   [casts it's collection view](https://referencesource.microsoft.com/#PresentationFramework/src/Framework/System/Windows/Controls/ItemCollection.cs,1481) to `IItemProperties`
3. [ListCollectionView.ItemProperties](https://referencesource.microsoft.com/#PresentationFramework/src/Framework/System/Windows/Data/ListCollectionView.cs,1625)
   invokes [CollectionView.GetItemProperties()](https://referencesource.microsoft.com/#PresentationFramework/src/Framework/System/Windows/Data/CollectionView.cs,1459)
4. `GetItemProperties` tries a few things, then if those don't work, casts
   the first item to an instance of `ICustomTypeProvider`

`ViewModel` can implement the `ICustomTypeProvider` interface by deriving from
`CustomTypeProvider` and overriding the `GetProperties` method to provide
property descriptors describing the bindings passed to its constructor.

In order for this to work, the `ObservableCollection` used needs to be changed
so that its item type is `obj` rather than `ViewModel<'model, 'msg>`, which
necessitates a few other changes (including some casting from `obj` to
`ViewModel<'model, 'msg>`). Otherwise the default `TypeDescriptor` for the
type will be found and used during the "tries a few things" section of stage 4.