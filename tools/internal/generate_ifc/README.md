# Generate Pascal code to express types defined in the IFC specification

The IFC standard, in the [Annex A](https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/annex-a.html), provides an [XML schema (xsd)](https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/IFC4X3_ADD2.xsd) that defines all the types (classes, enums) defined by the IFC standard.

This tool aims to generate Pascal code to express these types. It also downloads the required XSD file from the web, if it doesn't exist already.

It's not complete but it's a start and we use it to generate types inside the `src/scene/load/ifc/castleifc_auto_generated.inc`.
