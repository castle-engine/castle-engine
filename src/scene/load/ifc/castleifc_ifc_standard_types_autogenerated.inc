{%MainUnit castleifc.pas}
{ Auto-generated IFC classes definitions by generate_ifc. }
{$ifdef read_interface_types}
//type
  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSegment.htm IfcSegment). }
  TIfcSegment = class abstract(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC internal class IfcGeometricRepresentationSubContext-temp (not in specification, only in XSD). }
  TIfcGeometricRepresentationSubContext_Temp = class abstract(TIfcGeometricRepresentationContext)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeometricSet.htm IfcGeometricSet). }
  TIfcGeometricSet = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSectionedSpine.htm IfcSectionedSpine). }
  TIfcSectionedSpine = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeotechnicalElement.htm IfcGeotechnicalElement). }
  TIfcGeotechnicalElement = class abstract(TIfcElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSectionedSolid.htm IfcSectionedSolid). }
  TIfcSectionedSolid = class abstract(TIfcSolidModel)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGridPlacement.htm IfcGridPlacement). }
  TIfcGridPlacement = class(TIfcObjectPlacement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGroup.htm IfcGroup). }
  TIfcGroup = class(TIfcObject)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcHalfSpaceSolid.htm IfcHalfSpaceSolid). }
  TIfcHalfSpaceSolid = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcHeatExchanger.htm IfcHeatExchanger). }
  TIfcHeatExchanger = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcHeatExchangerType.htm IfcHeatExchangerType). }
  TIfcHeatExchangerType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcHumidifier.htm IfcHumidifier). }
  TIfcHumidifier = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowMovingDevice.htm IfcFlowMovingDevice). }
  TIfcFlowMovingDevice = class(TIfcDistributionFlowElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowMovingDeviceType.htm IfcFlowMovingDeviceType). }
  TIfcFlowMovingDeviceType = class abstract(TIfcDistributionFlowElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowSegment.htm IfcFlowSegment). }
  TIfcFlowSegment = class(TIfcDistributionFlowElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowSegmentType.htm IfcFlowSegmentType). }
  TIfcFlowSegmentType = class abstract(TIfcDistributionFlowElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowStorageDevice.htm IfcFlowStorageDevice). }
  TIfcFlowStorageDevice = class(TIfcDistributionFlowElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowStorageDeviceType.htm IfcFlowStorageDeviceType). }
  TIfcFlowStorageDeviceType = class abstract(TIfcDistributionFlowElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowTreatmentDevice.htm IfcFlowTreatmentDevice). }
  TIfcFlowTreatmentDevice = class(TIfcDistributionFlowElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowTreatmentDeviceType.htm IfcFlowTreatmentDeviceType). }
  TIfcFlowTreatmentDeviceType = class abstract(TIfcDistributionFlowElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFooting.htm IfcFooting). }
  TIfcFooting = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFootingType.htm IfcFootingType). }
  TIfcFootingType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeographicElement.htm IfcGeographicElement). }
  TIfcGeographicElement = class(TIfcElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeographicElementType.htm IfcGeographicElementType). }
  TIfcGeographicElementType = class(TIfcElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcKerb.htm IfcKerb). }
  TIfcKerb = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcKerbType.htm IfcKerbType). }
  TIfcKerbType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLShapeProfileDef.htm IfcLShapeProfileDef). }
  TIfcLShapeProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoof.htm IfcRoof). }
  TIfcRoof = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoad.htm IfcRoad). }
  TIfcRoad = class(TIfcFacility)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLamp.htm IfcLamp). }
  TIfcLamp = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLampType.htm IfcLampType). }
  TIfcLampType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLibraryReference.htm IfcLibraryReference). }
  TIfcLibraryReference = class(TIfcExternalReference)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightDistributionData.htm IfcLightDistributionData). }
  TIfcLightDistributionData = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightFixture.htm IfcLightFixture). }
  TIfcLightFixture = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightFixtureType.htm IfcLightFixtureType). }
  TIfcLightFixtureType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightIntensityDistribution.htm IfcLightIntensityDistribution). }
  TIfcLightIntensityDistribution = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcHumidifierType.htm IfcHumidifierType). }
  TIfcHumidifierType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIShapeProfileDef.htm IfcIShapeProfileDef). }
  TIfcIShapeProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcImpactProtectionDevice.htm IfcImpactProtectionDevice). }
  TIfcImpactProtectionDevice = class(TIfcElementComponent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcImpactProtectionDeviceType.htm IfcImpactProtectionDeviceType). }
  TIfcImpactProtectionDeviceType = class(TIfcElementComponentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedColourMap.htm IfcIndexedColourMap). }
  TIfcIndexedColourMap = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedPolygonalFaceWithVoids.htm IfcIndexedPolygonalFaceWithVoids). }
  TIfcIndexedPolygonalFaceWithVoids = class(TIfcIndexedPolygonalFace)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIndexedTriangleTextureMap.htm IfcIndexedTriangleTextureMap). }
  TIfcIndexedTriangleTextureMap = class(TIfcIndexedTextureMap)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSchedulingTime.htm IfcSchedulingTime). }
  TIfcSchedulingTime = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC internal class IfcSIUnit-temp (not in specification, only in XSD). }
  TIfcSIUnit_Temp = class abstract(TIfcNamedUnit)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIrregularTimeSeriesValue.htm IfcIrregularTimeSeriesValue). }
  TIfcIrregularTimeSeriesValue = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoundedRectangleProfileDef.htm IfcRoundedRectangleProfileDef). }
  TIfcRoundedRectangleProfileDef = class(TIfcRectangleProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoofType.htm IfcRoofType). }
  TIfcRoofType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialElementType.htm IfcSpatialElementType). }
  TIfcSpatialElementType = class abstract(TIfcTypeProduct)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpaceHeaterType.htm IfcSpaceHeaterType). }
  TIfcSpaceHeaterType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExternalInformation.htm IfcExternalInformation). }
  TIfcExternalInformation = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpaceHeater.htm IfcSpaceHeater). }
  TIfcSpaceHeater = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExternalSpatialStructureElement.htm IfcExternalSpatialStructureElement). }
  TIfcExternalSpatialStructureElement = class abstract(TIfcSpatialElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExternallyDefinedHatchStyle.htm IfcExternallyDefinedHatchStyle). }
  TIfcExternallyDefinedHatchStyle = class(TIfcExternalReference)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExternallyDefinedSurfaceStyle.htm IfcExternallyDefinedSurfaceStyle). }
  TIfcExternallyDefinedSurfaceStyle = class(TIfcExternalReference)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExternallyDefinedTextFont.htm IfcExternallyDefinedTextFont). }
  TIfcExternallyDefinedTextFont = class(TIfcExternalReference)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExtrudedAreaSolidTapered.htm IfcExtrudedAreaSolidTapered). }
  TIfcExtrudedAreaSolidTapered = class(TIfcExtrudedAreaSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpace.htm IfcSpace). }
  TIfcSpace = class(TIfcSpatialStructureElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFaceBasedSurfaceModel.htm IfcFaceBasedSurfaceModel). }
  TIfcFaceBasedSurfaceModel = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSolarDeviceType.htm IfcSolarDeviceType). }
  TIfcSolarDeviceType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricGeneratorType.htm IfcElectricGeneratorType). }
  TIfcElectricGeneratorType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricMotor.htm IfcElectricMotor). }
  TIfcElectricMotor = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricMotorType.htm IfcElectricMotorType). }
  TIfcElectricMotorType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStackTerminal.htm IfcStackTerminal). }
  TIfcStackTerminal = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpiral.htm IfcSpiral). }
  TIfcSpiral = class abstract(TIfcCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEllipseProfileDef.htm IfcEllipseProfileDef). }
  TIfcEllipseProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEngine.htm IfcEngine). }
  TIfcEngine = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEngineType.htm IfcEngineType). }
  TIfcEngineType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEvaporativeCooler.htm IfcEvaporativeCooler). }
  TIfcEvaporativeCooler = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEvaporativeCoolerType.htm IfcEvaporativeCoolerType). }
  TIfcEvaporativeCoolerType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEvaporator.htm IfcEvaporator). }
  TIfcEvaporator = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEvaporatorType.htm IfcEvaporatorType). }
  TIfcEvaporatorType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFillAreaStyleTiles.htm IfcFillAreaStyleTiles). }
  TIfcFillAreaStyleTiles = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcShellBasedSurfaceModel.htm IfcShellBasedSurfaceModel). }
  TIfcShellBasedSurfaceModel = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcShadingDeviceType.htm IfcShadingDeviceType). }
  TIfcShadingDeviceType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFireSuppressionTerminal.htm IfcFireSuppressionTerminal). }
  TIfcFireSuppressionTerminal = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFireSuppressionTerminalType.htm IfcFireSuppressionTerminalType). }
  TIfcFireSuppressionTerminalType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcShadingDevice.htm IfcShadingDevice). }
  TIfcShadingDevice = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowController.htm IfcFlowController). }
  TIfcFlowController = class(TIfcDistributionFlowElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowControllerType.htm IfcFlowControllerType). }
  TIfcFlowControllerType = class abstract(TIfcDistributionFlowElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowFitting.htm IfcFlowFitting). }
  TIfcFlowFitting = class(TIfcDistributionFlowElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowFittingType.htm IfcFlowFittingType). }
  TIfcFlowFittingType = class abstract(TIfcDistributionFlowElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowInstrumentType.htm IfcFlowInstrumentType). }
  TIfcFlowInstrumentType = class(TIfcDistributionControlElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSensorType.htm IfcSensorType). }
  TIfcSensorType = class(TIfcDistributionControlElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSolarDevice.htm IfcSolarDevice). }
  TIfcSolarDevice = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSimplePropertyTemplate.htm IfcSimplePropertyTemplate). }
  TIfcSimplePropertyTemplate = class(TIfcPropertyTemplate)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFacilityPart.htm IfcFacilityPart). }
  TIfcFacilityPart = class abstract(TIfcSpatialStructureElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSignalType.htm IfcSignalType). }
  TIfcSignalType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSignal.htm IfcSignal). }
  TIfcSignal = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSignType.htm IfcSignType). }
  TIfcSignType = class(TIfcElementComponentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSign.htm IfcSign). }
  TIfcSign = class(TIfcElementComponent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFastener.htm IfcFastener). }
  TIfcFastener = class(TIfcElementComponent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFastenerType.htm IfcFastenerType). }
  TIfcFastenerType = class(TIfcElementComponentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFeatureElementAddition.htm IfcFeatureElementAddition). }
  TIfcFeatureElementAddition = class abstract(TIfcFeatureElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFillAreaStyle.htm IfcFillAreaStyle). }
  TIfcFillAreaStyle = class(TIfcPresentationStyle)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFillAreaStyleHatching.htm IfcFillAreaStyleHatching). }
  TIfcFillAreaStyleHatching = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlate.htm IfcPlate). }
  TIfcPlate = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlateType.htm IfcPlateType). }
  TIfcPlateType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPointByDistanceExpression.htm IfcPointByDistanceExpression). }
  TIfcPointByDistanceExpression = class(TIfcPoint)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPointOnCurve.htm IfcPointOnCurve). }
  TIfcPointOnCurve = class(TIfcPoint)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPointOnSurface.htm IfcPointOnSurface). }
  TIfcPointOnSurface = class(TIfcPoint)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssigns.htm IfcRelAssigns). }
  TIfcRelAssigns = class abstract(TIfcRelationship)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcingMeshType.htm IfcReinforcingMeshType). }
  TIfcReinforcingMeshType = class(TIfcReinforcingElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPolynomialCurve.htm IfcPolynomialCurve). }
  TIfcPolynomialCurve = class(TIfcCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPort.htm IfcPort). }
  TIfcPort = class abstract(TIfcProduct)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcingMesh.htm IfcReinforcingMesh). }
  TIfcReinforcingMesh = class(TIfcReinforcingElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPreDefinedItem.htm IfcPreDefinedItem). }
  TIfcPreDefinedItem = class abstract(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPreDefinedProperties.htm IfcPreDefinedProperties). }
  TIfcPreDefinedProperties = class abstract(TIfcPropertyAbstraction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPavementType.htm IfcPavementType). }
  TIfcPavementType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPcurve.htm IfcPcurve). }
  TIfcPcurve = class(TIfcCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelConnectsPathElements.htm IfcRelConnectsPathElements). }
  TIfcRelConnectsPathElements = class(TIfcRelConnectsElements)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssociatesProfileDef.htm IfcRelAssociatesProfileDef). }
  TIfcRelAssociatesProfileDef = class(TIfcRelAssociates)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssociatesLibrary.htm IfcRelAssociatesLibrary). }
  TIfcRelAssociatesLibrary = class(TIfcRelAssociates)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssociatesDocument.htm IfcRelAssociatesDocument). }
  TIfcRelAssociatesDocument = class(TIfcRelAssociates)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPhysicalQuantity.htm IfcPhysicalQuantity). }
  TIfcPhysicalQuantity = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssociatesConstraint.htm IfcRelAssociatesConstraint). }
  TIfcRelAssociatesConstraint = class(TIfcRelAssociates)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssociatesClassification.htm IfcRelAssociatesClassification). }
  TIfcRelAssociatesClassification = class(TIfcRelAssociates)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssociatesApproval.htm IfcRelAssociatesApproval). }
  TIfcRelAssociatesApproval = class(TIfcRelAssociates)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPixelTexture.htm IfcPixelTexture). }
  TIfcPixelTexture = class(TIfcSurfaceTexture)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlanarExtent.htm IfcPlanarExtent). }
  TIfcPlanarExtent = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRectangleHollowProfileDef.htm IfcRectangleHollowProfileDef). }
  TIfcRectangleHollowProfileDef = class(TIfcRectangleProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProtectiveDeviceTrippingUnitType.htm IfcProtectiveDeviceTrippingUnitType). }
  TIfcProtectiveDeviceTrippingUnitType = class(TIfcDistributionControlElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRampType.htm IfcRampType). }
  TIfcRampType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRampFlightType.htm IfcRampFlightType). }
  TIfcRampFlightType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRampFlight.htm IfcRampFlight). }
  TIfcRampFlight = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRamp.htm IfcRamp). }
  TIfcRamp = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRailway.htm IfcRailway). }
  TIfcRailway = class(TIfcFacility)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRailingType.htm IfcRailingType). }
  TIfcRailingType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantitySet.htm IfcQuantitySet). }
  TIfcQuantitySet = class abstract(TIfcPropertySetDefinition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRailing.htm IfcRailing). }
  TIfcRailing = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRailType.htm IfcRailType). }
  TIfcRailType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRail.htm IfcRail). }
  TIfcRail = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPreDefinedPropertySet.htm IfcPreDefinedPropertySet). }
  TIfcPreDefinedPropertySet = class abstract(TIfcPropertySetDefinition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPresentationLayerWithStyle.htm IfcPresentationLayerWithStyle). }
  TIfcPresentationLayerWithStyle = class(TIfcPresentationLayerAssignment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProcess.htm IfcProcess). }
  TIfcProcess = class abstract(TIfcObject)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReferent.htm IfcReferent). }
  TIfcReferent = class(TIfcPositioningElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReference.htm IfcReference). }
  TIfcReference = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRecurrencePattern.htm IfcRecurrencePattern). }
  TIfcRecurrencePattern = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyBoundedValue.htm IfcPropertyBoundedValue). }
  TIfcPropertyBoundedValue = class(TIfcSimpleProperty)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyEnumeratedValue.htm IfcPropertyEnumeratedValue). }
  TIfcPropertyEnumeratedValue = class(TIfcSimpleProperty)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyEnumeration.htm IfcPropertyEnumeration). }
  TIfcPropertyEnumeration = class(TIfcPropertyAbstraction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyListValue.htm IfcPropertyListValue). }
  TIfcPropertyListValue = class(TIfcSimpleProperty)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyReferenceValue.htm IfcPropertyReferenceValue). }
  TIfcPropertyReferenceValue = class(TIfcSimpleProperty)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPropertyTableValue.htm IfcPropertyTableValue). }
  TIfcPropertyTableValue = class(TIfcSimpleProperty)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMarineFacility.htm IfcMarineFacility). }
  TIfcMarineFacility = class(TIfcFacility)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialClassificationRelationship.htm IfcMaterialClassificationRelationship). }
  TIfcMaterialClassificationRelationship = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialLayerWithOffsets.htm IfcMaterialLayerWithOffsets). }
  TIfcMaterialLayerWithOffsets = class(TIfcMaterialLayer)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialList.htm IfcMaterialList). }
  TIfcMaterialList_Spec = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialProfileSetUsageTapering.htm IfcMaterialProfileSetUsageTapering). }
  TIfcMaterialProfileSetUsageTapering = class(TIfcMaterialProfileSetUsage)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMaterialProfileWithOffsets.htm IfcMaterialProfileWithOffsets). }
  TIfcMaterialProfileWithOffsets = class(TIfcMaterialProfile)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMechanicalFastener.htm IfcMechanicalFastener). }
  TIfcMechanicalFastener = class(TIfcElementComponent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMechanicalFastenerType.htm IfcMechanicalFastenerType). }
  TIfcMechanicalFastenerType = class(TIfcElementComponentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMedicalDevice.htm IfcMedicalDevice). }
  TIfcMedicalDevice = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMedicalDeviceType.htm IfcMedicalDeviceType). }
  TIfcMedicalDeviceType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMember.htm IfcMember). }
  TIfcMember = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMemberType.htm IfcMemberType). }
  TIfcMemberType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightSource.htm IfcLightSource). }
  TIfcLightSource = class abstract(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRevolvedAreaSolid.htm IfcRevolvedAreaSolid). }
  TIfcRevolvedAreaSolid = class(TIfcSweptAreaSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLine.htm IfcLine). }
  TIfcLine = class(TIfcCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLinearElement.htm IfcLinearElement). }
  TIfcLinearElement = class(TIfcProduct)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLinearPlacement.htm IfcLinearPlacement). }
  TIfcLinearPlacement = class(TIfcObjectPlacement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLinearPositioningElement.htm IfcLinearPositioningElement). }
  TIfcLinearPositioningElement = class(TIfcPositioningElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLiquidTerminal.htm IfcLiquidTerminal). }
  TIfcLiquidTerminal = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLiquidTerminalType.htm IfcLiquidTerminalType). }
  TIfcLiquidTerminalType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcResourceConstraintRelationship.htm IfcResourceConstraintRelationship). }
  TIfcResourceConstraintRelationship = class(TIfcResourceLevelRelationship)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcManifoldSolidBrep.htm IfcManifoldSolidBrep). }
  TIfcManifoldSolidBrep = class abstract(TIfcSolidModel)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcResourceApprovalRelationship.htm IfcResourceApprovalRelationship). }
  TIfcResourceApprovalRelationship = class(TIfcResourceLevelRelationship)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcResource.htm IfcResource). }
  TIfcResource = class abstract(TIfcObject)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOffsetCurve.htm IfcOffsetCurve). }
  TIfcOffsetCurve = class abstract(TIfcCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelNests.htm IfcRelNests). }
  TIfcRelNests = class(TIfcRelDecomposes)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelFlowControlElements.htm IfcRelFlowControlElements). }
  TIfcRelFlowControlElements = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelCoversSpaces.htm IfcRelCoversSpaces). }
  TIfcRelCoversSpaces = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOpenCrossProfileDef.htm IfcOpenCrossProfileDef). }
  TIfcOpenCrossProfileDef = class(TIfcProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelConnectsStructuralMember.htm IfcRelConnectsStructuralMember). }
  TIfcRelConnectsStructuralMember = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelConnectsStructuralActivity.htm IfcRelConnectsStructuralActivity). }
  TIfcRelConnectsStructuralActivity = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelConnectsPorts.htm IfcRelConnectsPorts). }
  TIfcRelConnectsPorts = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOutlet.htm IfcOutlet). }
  TIfcOutlet = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOutletType.htm IfcOutletType). }
  TIfcOutletType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelConnectsPortToElement.htm IfcRelConnectsPortToElement). }
  TIfcRelConnectsPortToElement = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPavement.htm IfcPavement). }
  TIfcPavement = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelSpaceBoundary1stLevel.htm IfcRelSpaceBoundary1stLevel). }
  TIfcRelSpaceBoundary1stLevel = class(TIfcRelSpaceBoundary)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelServicesBuildings.htm IfcRelServicesBuildings). }
  TIfcRelServicesBuildings = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMobileTelecommunicationsAppliance.htm IfcMobileTelecommunicationsAppliance). }
  TIfcMobileTelecommunicationsAppliance = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMobileTelecommunicationsApplianceType.htm IfcMobileTelecommunicationsApplianceType). }
  TIfcMobileTelecommunicationsApplianceType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMooringDevice.htm IfcMooringDevice). }
  TIfcMooringDevice = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMooringDeviceType.htm IfcMooringDeviceType). }
  TIfcMooringDeviceType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMotorConnection.htm IfcMotorConnection). }
  TIfcMotorConnection = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMotorConnectionType.htm IfcMotorConnectionType). }
  TIfcMotorConnectionType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcNavigationElement.htm IfcNavigationElement). }
  TIfcNavigationElement = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcNavigationElementType.htm IfcNavigationElementType). }
  TIfcNavigationElementType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelSequence.htm IfcRelSequence). }
  TIfcRelSequence = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelReferencedInSpatialStructure.htm IfcRelReferencedInSpatialStructure). }
  TIfcRelReferencedInSpatialStructure = class(TIfcRelConnects)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTimeSeries.htm IfcTimeSeries). }
  TIfcTimeSeries = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCircleProfileDef.htm IfcCircleProfileDef). }
  TIfcCircleProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCivilElement.htm IfcCivilElement). }
  TIfcCivilElement = class(TIfcElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCivilElementType.htm IfcCivilElementType). }
  TIfcCivilElementType = class(TIfcElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTimePeriod.htm IfcTimePeriod). }
  TIfcTimePeriod = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcClassificationReference.htm IfcClassificationReference). }
  TIfcClassificationReference = class(TIfcExternalReference)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextureVertex.htm IfcTextureVertex). }
  TIfcTextureVertex = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCoil.htm IfcCoil). }
  TIfcCoil = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCoilType.htm IfcCoilType). }
  TIfcCoilType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextureMap.htm IfcTextureMap). }
  TIfcTextureMap = class(TIfcTextureCoordinate)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcColourRgbList.htm IfcColourRgbList). }
  TIfcColourRgbList_Spec = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcChimneyType.htm IfcChimneyType). }
  TIfcChimneyType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTransformerType.htm IfcTransformerType). }
  TIfcTransformerType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTransformer.htm IfcTransformer). }
  TIfcTransformer = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTrackElementType.htm IfcTrackElementType). }
  TIfcTrackElementType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTrackElement.htm IfcTrackElement). }
  TIfcTrackElement = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTopologyRepresentation.htm IfcTopologyRepresentation). }
  TIfcTopologyRepresentation = class(TIfcShapeModel)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTopologicalRepresentationItem.htm IfcTopologicalRepresentationItem). }
  TIfcTopologicalRepresentationItem = class abstract(TIfcRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCartesianTransformationOperator2DnonUniform.htm IfcCartesianTransformationOperator2DnonUniform). }
  TIfcCartesianTransformationOperator2DnonUniform = class(TIfcCartesianTransformationOperator2D)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTimeSeriesValue.htm IfcTimeSeriesValue). }
  TIfcTimeSeriesValue = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcChiller.htm IfcChiller). }
  TIfcChiller = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcChillerType.htm IfcChillerType). }
  TIfcChillerType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcChimney.htm IfcChimney). }
  TIfcChimney = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcColourSpecification.htm IfcColourSpecification). }
  TIfcColourSpecification = class abstract(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConnectionCurveGeometry.htm IfcConnectionCurveGeometry). }
  TIfcConnectionCurveGeometry = class(TIfcConnectionGeometry)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConnectionPointGeometry.htm IfcConnectionPointGeometry). }
  TIfcConnectionPointGeometry = class(TIfcConnectionGeometry)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConnectionSurfaceGeometry.htm IfcConnectionSurfaceGeometry). }
  TIfcConnectionSurfaceGeometry = class(TIfcConnectionGeometry)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConnectionVolumeGeometry.htm IfcConnectionVolumeGeometry). }
  TIfcConnectionVolumeGeometry = class(TIfcConnectionGeometry)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstraint.htm IfcConstraint). }
  TIfcConstraint = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextLiteral.htm IfcTextLiteral). }
  TIfcTextLiteral = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTendonType.htm IfcTendonType). }
  TIfcTendonType = class(TIfcReinforcingElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTendonConduitType.htm IfcTendonConduitType). }
  TIfcTendonConduitType = class(TIfcReinforcingElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTendonConduit.htm IfcTendonConduit). }
  TIfcTendonConduit = class(TIfcReinforcingElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTendonAnchorType.htm IfcTendonAnchorType). }
  TIfcTendonAnchorType = class(TIfcReinforcingElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTendonAnchor.htm IfcTendonAnchor). }
  TIfcTendonAnchor = class(TIfcReinforcingElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextStyle.htm IfcTextStyle). }
  TIfcTextStyle = class(TIfcPresentationStyle)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcColumnType.htm IfcColumnType). }
  TIfcColumnType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCommunicationsAppliance.htm IfcCommunicationsAppliance). }
  TIfcCommunicationsAppliance = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCommunicationsApplianceType.htm IfcCommunicationsApplianceType). }
  TIfcCommunicationsApplianceType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcComplexPropertyTemplate.htm IfcComplexPropertyTemplate). }
  TIfcComplexPropertyTemplate = class(TIfcPropertyTemplate)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCompositeCurve.htm IfcCompositeCurve). }
  TIfcCompositeCurve = class(TIfcBoundedCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextureCoordinateIndicesWithVoids.htm IfcTextureCoordinateIndicesWithVoids). }
  TIfcTextureCoordinateIndicesWithVoids = class(TIfcTextureCoordinateIndices)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextureCoordinateGenerator.htm IfcTextureCoordinateGenerator). }
  TIfcTextureCoordinateGenerator = class(TIfcTextureCoordinate)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextStyleTextModel.htm IfcTextStyleTextModel). }
  TIfcTextStyleTextModel = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextStyleForDefinedFont.htm IfcTextStyleForDefinedFont). }
  TIfcTextStyleForDefinedFont = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCondenser.htm IfcCondenser). }
  TIfcCondenser = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCondenserType.htm IfcCondenserType). }
  TIfcCondenserType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTransportationDevice.htm IfcTransportationDevice). }
  TIfcTransportationDevice = class abstract(TIfcElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentParameterSegment.htm IfcAlignmentParameterSegment). }
  TIfcAlignmentParameterSegment = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVibrationDamperType.htm IfcVibrationDamperType). }
  TIfcVibrationDamperType = class(TIfcElementComponentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVibrationDamper.htm IfcVibrationDamper). }
  TIfcVibrationDamper = class(TIfcElementComponent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAnnotation.htm IfcAnnotation). }
  TIfcAnnotation = class(TIfcProduct)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAnnotationFillArea.htm IfcAnnotationFillArea). }
  TIfcAnnotationFillArea = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAppliedValue.htm IfcAppliedValue). }
  TIfcAppliedValue = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcApproval.htm IfcApproval). }
  TIfcApproval = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcApprovalRelationship.htm IfcApprovalRelationship). }
  TIfcApprovalRelationship = class(TIfcResourceLevelRelationship)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcArbitraryOpenProfileDef.htm IfcArbitraryOpenProfileDef). }
  TIfcArbitraryOpenProfileDef = class(TIfcProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAsymmetricIShapeProfileDef.htm IfcAsymmetricIShapeProfileDef). }
  TIfcAsymmetricIShapeProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAudioVisualAppliance.htm IfcAudioVisualAppliance). }
  TIfcAudioVisualAppliance = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVibrationIsolator.htm IfcVibrationIsolator). }
  TIfcVibrationIsolator = class(TIfcElementComponent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcZShapeProfileDef.htm IfcZShapeProfileDef). }
  TIfcZShapeProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcActor.htm IfcActor). }
  TIfcActor = class(TIfcObject)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWellKnownText.htm IfcWellKnownText). }
  TIfcWellKnownText = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAirToAirHeatRecovery.htm IfcAirToAirHeatRecovery). }
  TIfcAirToAirHeatRecovery = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAirToAirHeatRecoveryType.htm IfcAirToAirHeatRecoveryType). }
  TIfcAirToAirHeatRecoveryType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWasteTerminalType.htm IfcWasteTerminalType). }
  TIfcWasteTerminalType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlarmType.htm IfcAlarmType). }
  TIfcAlarmType = class(TIfcDistributionControlElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWasteTerminal.htm IfcWasteTerminal). }
  TIfcWasteTerminal = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVoidingFeature.htm IfcVoidingFeature). }
  TIfcVoidingFeature = class(TIfcFeatureElementSubtraction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVirtualElement.htm IfcVirtualElement). }
  TIfcVirtualElement = class(TIfcElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVibrationIsolatorType.htm IfcVibrationIsolatorType). }
  TIfcVibrationIsolatorType = class(TIfcElementComponentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAudioVisualApplianceType.htm IfcAudioVisualApplianceType). }
  TIfcAudioVisualApplianceType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTypeResource.htm IfcTypeResource). }
  TIfcTypeResource = class abstract(TIfcTypeObject)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTypeProcess.htm IfcTypeProcess). }
  TIfcTypeProcess = class abstract(TIfcTypeObject)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTubeBundleType.htm IfcTubeBundleType). }
  TIfcTubeBundleType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBridge.htm IfcBridge). }
  TIfcBridge = class(TIfcFacility)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTubeBundle.htm IfcTubeBundle). }
  TIfcTubeBundle = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTriangulatedIrregularNetwork.htm IfcTriangulatedIrregularNetwork). }
  TIfcTriangulatedIrregularNetwork = class(TIfcTriangulatedFaceSet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTrapeziumProfileDef.htm IfcTrapeziumProfileDef). }
  TIfcTrapeziumProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBurner.htm IfcBurner). }
  TIfcBurner = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBurnerType.htm IfcBurnerType). }
  TIfcBurnerType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCShapeProfileDef.htm IfcCShapeProfileDef). }
  TIfcCShapeProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTransportationDeviceType.htm IfcTransportationDeviceType). }
  TIfcTransportationDeviceType = class abstract(TIfcElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcUShapeProfileDef.htm IfcUShapeProfileDef). }
  TIfcUShapeProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis1Placement.htm IfcAxis1Placement). }
  TIfcAxis1Placement = class(TIfcPlacement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAxis2PlacementLinear.htm IfcAxis2PlacementLinear). }
  TIfcAxis2PlacementLinear = class(TIfcPlacement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBSplineCurve.htm IfcBSplineCurve). }
  TIfcBSplineCurve = class abstract(TIfcBoundedCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVector.htm IfcVector). }
  TIfcVector = class(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBearing.htm IfcBearing). }
  TIfcBearing = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBearingType.htm IfcBearingType). }
  TIfcBearingType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBlobTexture.htm IfcBlobTexture). }
  TIfcBlobTexture = class(TIfcSurfaceTexture)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcUnitaryEquipmentType.htm IfcUnitaryEquipmentType). }
  TIfcUnitaryEquipmentType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoundaryCondition.htm IfcBoundaryCondition). }
  TIfcBoundaryCondition = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcUnitaryEquipment.htm IfcUnitaryEquipment). }
  TIfcUnitaryEquipment = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcUnitaryControlElementType.htm IfcUnitaryControlElementType). }
  TIfcUnitaryControlElementType = class(TIfcDistributionControlElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTendon.htm IfcTendon). }
  TIfcTendon = class(TIfcReinforcingElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceStyle.htm IfcSurfaceStyle). }
  TIfcSurfaceStyle = class(TIfcPresentationStyle)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionChamberElementType.htm IfcDistributionChamberElementType). }
  TIfcDistributionChamberElementType = class(TIfcDistributionFlowElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionChamberElement.htm IfcDistributionChamberElement). }
  TIfcDistributionChamberElement = class(TIfcDistributionFlowElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceStyleLighting.htm IfcSurfaceStyleLighting). }
  TIfcSurfaceStyleLighting = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceStyleRefraction.htm IfcSurfaceStyleRefraction). }
  TIfcSurfaceStyleRefraction = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDiscreteAccessoryType.htm IfcDiscreteAccessoryType). }
  TIfcDiscreteAccessoryType = class(TIfcElementComponentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDiscreteAccessory.htm IfcDiscreteAccessory). }
  TIfcDiscreteAccessory = class(TIfcElementComponent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDirectrixCurveSweptAreaSolid.htm IfcDirectrixCurveSweptAreaSolid). }
  TIfcDirectrixCurveSweptAreaSolid = class abstract(TIfcSweptAreaSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDerivedProfileDef.htm IfcDerivedProfileDef). }
  TIfcDerivedProfileDef = class(TIfcProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDeepFoundationType.htm IfcDeepFoundationType). }
  TIfcDeepFoundationType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDeepFoundation.htm IfcDeepFoundation). }
  TIfcDeepFoundation = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceStyleShading.htm IfcSurfaceStyleShading). }
  TIfcSurfaceStyleShading = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSweptDiskSolid.htm IfcSweptDiskSolid). }
  TIfcSweptDiskSolid = class(TIfcSolidModel)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurveStyleFontPattern.htm IfcCurveStyleFontPattern). }
  TIfcCurveStyleFontPattern = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurveStyleFontAndScaling.htm IfcCurveStyleFontAndScaling). }
  TIfcCurveStyleFontAndScaling = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurveStyleFont.htm IfcCurveStyleFont). }
  TIfcCurveStyleFont = class(TIfcPresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurveStyle.htm IfcCurveStyle). }
  TIfcCurveStyle = class(TIfcPresentationStyle)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurtainWallType.htm IfcCurtainWallType). }
  TIfcCurtainWallType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurtainWall.htm IfcCurtainWall). }
  TIfcCurtainWall = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurrencyRelationship.htm IfcCurrencyRelationship). }
  TIfcCurrencyRelationship = class(TIfcResourceLevelRelationship)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionControlElement.htm IfcDistributionControlElement). }
  TIfcDistributionControlElement = class(TIfcDistributionElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricGenerator.htm IfcElectricGenerator). }
  TIfcElectricGenerator = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStackTerminalType.htm IfcStackTerminalType). }
  TIfcStackTerminalType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStair.htm IfcStair). }
  TIfcStair = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStairFlight.htm IfcStairFlight). }
  TIfcStairFlight = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStairFlightType.htm IfcStairFlightType). }
  TIfcStairFlightType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStairType.htm IfcStairType). }
  TIfcStairType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricApplianceType.htm IfcElectricApplianceType). }
  TIfcElectricApplianceType = class(TIfcFlowTerminalType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricAppliance.htm IfcElectricAppliance). }
  TIfcElectricAppliance = class(TIfcFlowTerminal)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralActivity.htm IfcStructuralActivity). }
  TIfcStructuralActivity = class abstract(TIfcProduct)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralConnectionCondition.htm IfcStructuralConnectionCondition). }
  TIfcStructuralConnectionCondition = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralItem.htm IfcStructuralItem). }
  TIfcStructuralItem = class abstract(TIfcProduct)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEarthworksElement.htm IfcEarthworksElement). }
  TIfcEarthworksElement = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEarthworksCut.htm IfcEarthworksCut). }
  TIfcEarthworksCut = class(TIfcFeatureElementSubtraction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoad.htm IfcStructuralLoad). }
  TIfcStructuralLoad = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStyleModel.htm IfcStyleModel). }
  TIfcStyleModel = class abstract(TIfcRepresentation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurface.htm IfcSurface). }
  TIfcSurface = class abstract(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDocumentReference.htm IfcDocumentReference). }
  TIfcDocumentReference = class(TIfcExternalReference)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDocumentInformationRelationship.htm IfcDocumentInformationRelationship). }
  TIfcDocumentInformationRelationship = class(TIfcResourceLevelRelationship)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceCurve.htm IfcSurfaceCurve). }
  TIfcSurfaceCurve = class(TIfcCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceFeature.htm IfcSurfaceFeature). }
  TIfcSurfaceFeature = class(TIfcFeatureElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCovering.htm IfcCovering). }
  TIfcCovering = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCourseType.htm IfcCourseType). }
  TIfcCourseType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCourse.htm IfcCourse). }
  TIfcCourse = class(TIfcBuiltElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcControl.htm IfcControl). }
  TIfcControl = class abstract(TIfcObject)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTable.htm IfcTable). }
  TIfcTable = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTableColumn.htm IfcTableColumn). }
  TIfcTableColumn = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTableRow.htm IfcTableRow). }
  TIfcTableRow = class(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCoordinateReferenceSystem.htm IfcCoordinateReferenceSystem). }
  TIfcCoordinateReferenceSystem = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcControllerType.htm IfcControllerType). }
  TIfcControllerType = class(TIfcDistributionControlElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCoordinateOperation.htm IfcCoordinateOperation). }
  TIfcCoordinateOperation = class abstract(TIfcPersistent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCoolingTowerType.htm IfcCoolingTowerType). }
  TIfcCoolingTowerType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCooledBeam.htm IfcCooledBeam). }
  TIfcCooledBeam = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCoolingTower.htm IfcCoolingTower). }
  TIfcCoolingTower = class(TIfcEnergyConversionDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCooledBeamType.htm IfcCooledBeamType). }
  TIfcCooledBeamType = class(TIfcEnergyConversionDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCsgSolid.htm IfcCsgSolid). }
  TIfcCsgSolid = class(TIfcSolidModel)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTShapeProfileDef.htm IfcTShapeProfileDef). }
  TIfcTShapeProfileDef = class(TIfcParameterizedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCsgPrimitive3D.htm IfcCsgPrimitive3D). }
  TIfcCsgPrimitive3D = class abstract(TIfcGeometricRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcContextDependentUnit.htm IfcContextDependentUnit). }
  TIfcContextDependentUnit = class(TIfcNamedUnit)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCoveringType.htm IfcCoveringType). }
  TIfcCoveringType = class(TIfcBuiltElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextLiteralWithExtent.htm IfcTextLiteralWithExtent). }
  TIfcTextLiteralWithExtent = class(TIfcTextLiteral)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcementBarProperties.htm IfcReinforcementBarProperties). }
  TIfcReinforcementBarProperties = class(TIfcPreDefinedProperties)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTankType.htm IfcTankType). }
  TIfcTankType = class(TIfcFlowStorageDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcedSoil.htm IfcReinforcedSoil). }
  TIfcReinforcedSoil = class(TIfcEarthworksElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRegularTimeSeries.htm IfcRegularTimeSeries). }
  TIfcRegularTimeSeries = class(TIfcTimeSeries)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRigidOperation.htm IfcRigidOperation). }
  TIfcRigidOperation = class(TIfcCoordinateOperation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVertex.htm IfcVertex). }
  TIfcVertex = class(TIfcTopologicalRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTaskType.htm IfcTaskType). }
  TIfcTaskType = class(TIfcTypeProcess)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadConfiguration.htm IfcStructuralLoadConfiguration). }
  TIfcStructuralLoadConfiguration = class(TIfcStructuralLoad)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReinforcementDefinitionProperties.htm IfcReinforcementDefinitionProperties). }
  TIfcReinforcementDefinitionProperties = class(TIfcPreDefinedPropertySet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadGroup.htm IfcStructuralLoadGroup). }
  TIfcStructuralLoadGroup = class(TIfcGroup)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVehicleType.htm IfcVehicleType). }
  TIfcVehicleType = class(TIfcTransportationDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadOrResult.htm IfcStructuralLoadOrResult). }
  TIfcStructuralLoadOrResult = class abstract(TIfcStructuralLoad)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralMember.htm IfcStructuralMember). }
  TIfcStructuralMember = class abstract(TIfcStructuralItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralReaction.htm IfcStructuralReaction). }
  TIfcStructuralReaction = class abstract(TIfcStructuralActivity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSecondOrderPolynomialSpiral.htm IfcSecondOrderPolynomialSpiral). }
  TIfcSecondOrderPolynomialSpiral = class(TIfcSpiral)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSectionProperties.htm IfcSectionProperties). }
  TIfcSectionProperties = class(TIfcPreDefinedProperties)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSphere.htm IfcSphere). }
  TIfcSphere = class(TIfcCsgPrimitive3D)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSeamCurve.htm IfcSeamCurve). }
  TIfcSeamCurve = class(TIfcSurfaceCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialZoneType.htm IfcSpatialZoneType). }
  TIfcSpatialZoneType = class(TIfcSpatialElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWindowLiningProperties.htm IfcWindowLiningProperties). }
  TIfcWindowLiningProperties = class(TIfcPreDefinedPropertySet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRectangularPyramid.htm IfcRectangularPyramid). }
  TIfcRectangularPyramid = class(TIfcCsgPrimitive3D)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralAction.htm IfcStructuralAction). }
  TIfcStructuralAction = class abstract(TIfcStructuralActivity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRailwayPart.htm IfcRailwayPart). }
  TIfcRailwayPart = class(TIfcFacilityPart)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRoadPart.htm IfcRoadPart). }
  TIfcRoadPart = class(TIfcFacilityPart)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWindowPanelProperties.htm IfcWindowPanelProperties). }
  TIfcWindowPanelProperties = class(TIfcPreDefinedPropertySet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTaskTime.htm IfcTaskTime). }
  TIfcTaskTime = class(TIfcSchedulingTime)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWorkCalendar.htm IfcWorkCalendar). }
  TIfcWorkCalendar = class(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWorkControl.htm IfcWorkControl). }
  TIfcWorkControl = class abstract(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWorkTime.htm IfcWorkTime). }
  TIfcWorkTime = class(TIfcSchedulingTime)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralConnection.htm IfcStructuralConnection). }
  TIfcStructuralConnection = class abstract(TIfcStructuralItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTask.htm IfcTask). }
  TIfcTask = class(TIfcProcess)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpatialStructureElementType.htm IfcSpatialStructureElementType). }
  TIfcSpatialStructureElementType = class abstract(TIfcSpatialElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTank.htm IfcTank). }
  TIfcTank = class(TIfcFlowStorageDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSectionReinforcementProperties.htm IfcSectionReinforcementProperties). }
  TIfcSectionReinforcementProperties = class(TIfcPreDefinedProperties)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceStyleRendering.htm IfcSurfaceStyleRendering). }
  TIfcSurfaceStyleRendering = class(TIfcSurfaceStyleShading)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelConnectsWithEccentricity.htm IfcRelConnectsWithEccentricity). }
  TIfcRelConnectsWithEccentricity = class(TIfcRelConnectsStructuralMember)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSectionedSolidHorizontal.htm IfcSectionedSolidHorizontal). }
  TIfcSectionedSolidHorizontal = class(TIfcSectionedSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSectionedSurface.htm IfcSectionedSurface). }
  TIfcSectionedSurface = class(TIfcSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTransportElementType.htm IfcTransportElementType). }
  TIfcTransportElementType = class(TIfcTransportationDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTransportElement.htm IfcTransportElement). }
  TIfcTransportElement = class(TIfcTransportationDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelSpaceBoundary2ndLevel.htm IfcRelSpaceBoundary2ndLevel). }
  TIfcRelSpaceBoundary2ndLevel = class(TIfcRelSpaceBoundary1stLevel)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSweptDiskSolidPolygonal.htm IfcSweptDiskSolidPolygonal). }
  TIfcSweptDiskSolidPolygonal = class(TIfcSweptDiskSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSeventhOrderPolynomialSpiral.htm IfcSeventhOrderPolynomialSpiral). }
  TIfcSeventhOrderPolynomialSpiral = class(TIfcSpiral)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSensor.htm IfcSensor). }
  TIfcSensor = class(TIfcDistributionControlElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSystem.htm IfcSystem). }
  TIfcSystem = class(TIfcGroup)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcThirdOrderPolynomialSpiral.htm IfcThirdOrderPolynomialSpiral). }
  TIfcThirdOrderPolynomialSpiral = class(TIfcSpiral)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSweptSurface.htm IfcSweptSurface). }
  TIfcSweptSurface = class abstract(TIfcSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSwitchingDevice.htm IfcSwitchingDevice). }
  TIfcSwitchingDevice = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSwitchingDeviceType.htm IfcSwitchingDeviceType). }
  TIfcSwitchingDeviceType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSegmentedReferenceCurve.htm IfcSegmentedReferenceCurve). }
  TIfcSegmentedReferenceCurve = class(TIfcCompositeCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcResourceTime.htm IfcResourceTime). }
  TIfcResourceTime = class(TIfcSchedulingTime)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralResultGroup.htm IfcStructuralResultGroup). }
  TIfcStructuralResultGroup = class(TIfcGroup)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStyledRepresentation.htm IfcStyledRepresentation). }
  TIfcStyledRepresentation = class(TIfcStyleModel)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVehicle.htm IfcVehicle). }
  TIfcVehicle = class(TIfcTransportationDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssignsToActor.htm IfcRelAssignsToActor). }
  TIfcRelAssignsToActor = class(TIfcRelAssigns)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcValveType.htm IfcValveType). }
  TIfcValveType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssignsToControl.htm IfcRelAssignsToControl). }
  TIfcRelAssignsToControl = class(TIfcRelAssigns)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSlippageConnectionCondition.htm IfcSlippageConnectionCondition). }
  TIfcSlippageConnectionCondition = class(TIfcStructuralConnectionCondition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssignsToGroup.htm IfcRelAssignsToGroup). }
  TIfcRelAssignsToGroup = class(TIfcRelAssigns)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssignsToProcess.htm IfcRelAssignsToProcess). }
  TIfcRelAssignsToProcess = class(TIfcRelAssigns)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssignsToProduct.htm IfcRelAssignsToProduct). }
  TIfcRelAssignsToProduct = class(TIfcRelAssigns)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssignsToResource.htm IfcRelAssignsToResource). }
  TIfcRelAssignsToResource = class(TIfcRelAssigns)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSineSpiral.htm IfcSineSpiral). }
  TIfcSineSpiral = class(TIfcSpiral)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcValve.htm IfcValve). }
  TIfcValve = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRightCircularCylinder.htm IfcRightCircularCylinder). }
  TIfcRightCircularCylinder = class(TIfcCsgPrimitive3D)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRightCircularCone.htm IfcRightCircularCone). }
  TIfcRightCircularCone = class(TIfcCsgPrimitive3D)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceCurveSweptAreaSolid.htm IfcSurfaceCurveSweptAreaSolid). }
  TIfcSurfaceCurveSweptAreaSolid = class(TIfcDirectrixCurveSweptAreaSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRevolvedAreaSolidTapered.htm IfcRevolvedAreaSolidTapered). }
  TIfcRevolvedAreaSolidTapered = class(TIfcRevolvedAreaSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcUnitaryControlElement.htm IfcUnitaryControlElement). }
  TIfcUnitaryControlElement = class(TIfcDistributionControlElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoundaryNodeCondition.htm IfcBoundaryNodeCondition). }
  TIfcBoundaryNodeCondition = class(TIfcBoundaryCondition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricTimeControlType.htm IfcElectricTimeControlType). }
  TIfcElectricTimeControlType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricTimeControl.htm IfcElectricTimeControl). }
  TIfcElectricTimeControl = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricFlowTreatmentDeviceType.htm IfcElectricFlowTreatmentDeviceType). }
  TIfcElectricFlowTreatmentDeviceType = class(TIfcFlowTreatmentDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricFlowTreatmentDevice.htm IfcElectricFlowTreatmentDevice). }
  TIfcElectricFlowTreatmentDevice = class(TIfcFlowTreatmentDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricFlowStorageDeviceType.htm IfcElectricFlowStorageDeviceType). }
  TIfcElectricFlowStorageDeviceType = class(TIfcFlowStorageDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricFlowStorageDevice.htm IfcElectricFlowStorageDevice). }
  TIfcElectricFlowStorageDevice = class(TIfcFlowStorageDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricDistributionBoardType.htm IfcElectricDistributionBoardType). }
  TIfcElectricDistributionBoardType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElectricDistributionBoard.htm IfcElectricDistributionBoard). }
  TIfcElectricDistributionBoard = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoundedSurface.htm IfcBoundedSurface). }
  TIfcBoundedSurface = class abstract(TIfcSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoxedHalfSpace.htm IfcBoxedHalfSpace). }
  TIfcBoxedHalfSpace = class(TIfcHalfSpaceSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEdge.htm IfcEdge). }
  TIfcEdge = class(TIfcTopologicalRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBridgePart.htm IfcBridgePart). }
  TIfcBridgePart = class(TIfcFacilityPart)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableCarrierFitting.htm IfcCableCarrierFitting). }
  TIfcCableCarrierFitting = class(TIfcFlowFitting)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableCarrierFittingType.htm IfcCableCarrierFittingType). }
  TIfcCableCarrierFittingType = class(TIfcFlowFittingType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEarthworksFill.htm IfcEarthworksFill). }
  TIfcEarthworksFill = class(TIfcEarthworksElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableCarrierSegment.htm IfcCableCarrierSegment). }
  TIfcCableCarrierSegment = class(TIfcFlowSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDuctSilencerType.htm IfcDuctSilencerType). }
  TIfcDuctSilencerType = class(TIfcFlowTreatmentDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableCarrierSegmentType.htm IfcCableCarrierSegmentType). }
  TIfcCableCarrierSegmentType = class(TIfcFlowSegmentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElementQuantity.htm IfcElementQuantity). }
  TIfcElementQuantity = class(TIfcQuantitySet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowMeter.htm IfcFlowMeter). }
  TIfcFlowMeter = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowInstrument.htm IfcFlowInstrument). }
  TIfcFlowInstrument = class(TIfcDistributionControlElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFixedReferenceSweptAreaSolid.htm IfcFixedReferenceSweptAreaSolid). }
  TIfcFixedReferenceSweptAreaSolid = class(TIfcDirectrixCurveSweptAreaSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFilterType.htm IfcFilterType). }
  TIfcFilterType = class(TIfcFlowTreatmentDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFilter.htm IfcFilter). }
  TIfcFilter = class(TIfcFlowTreatmentDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFanType.htm IfcFanType). }
  TIfcFanType = class(TIfcFlowMovingDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFan.htm IfcFan). }
  TIfcFan = class(TIfcFlowMovingDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFailureConnectionCondition.htm IfcFailureConnectionCondition). }
  TIfcFailureConnectionCondition = class(TIfcStructuralConnectionCondition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFacilityPartCommon.htm IfcFacilityPartCommon). }
  TIfcFacilityPartCommon = class(TIfcFacilityPart)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFacetedBrep.htm IfcFacetedBrep). }
  TIfcFacetedBrep = class(TIfcManifoldSolidBrep)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoundaryEdgeCondition.htm IfcBoundaryEdgeCondition). }
  TIfcBoundaryEdgeCondition = class(TIfcBoundaryCondition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFaceBound.htm IfcFaceBound). }
  TIfcFaceBound = class(TIfcTopologicalRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFace.htm IfcFace). }
  TIfcFace = class(TIfcTopologicalRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcExternalSpatialElement.htm IfcExternalSpatialElement). }
  TIfcExternalSpatialElement = class(TIfcExternalSpatialStructureElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEventType.htm IfcEventType). }
  TIfcEventType = class(TIfcTypeProcess)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoundaryFaceCondition.htm IfcBoundaryFaceCondition). }
  TIfcBoundaryFaceCondition = class(TIfcBoundaryCondition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEventTime.htm IfcEventTime). }
  TIfcEventTime = class(TIfcSchedulingTime)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEvent.htm IfcEvent). }
  TIfcEvent = class(TIfcProcess)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcElementarySurface.htm IfcElementarySurface). }
  TIfcElementarySurface = class abstract(TIfcSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDuctSilencer.htm IfcDuctSilencer). }
  TIfcDuctSilencer = class(TIfcFlowTreatmentDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDoorPanelProperties.htm IfcDoorPanelProperties). }
  TIfcDoorPanelProperties = class(TIfcPreDefinedPropertySet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConnectionPointEccentricity.htm IfcConnectionPointEccentricity). }
  TIfcConnectionPointEccentricity = class(TIfcConnectionPointGeometry)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDoorLiningProperties.htm IfcDoorLiningProperties). }
  TIfcDoorLiningProperties = class(TIfcPreDefinedPropertySet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDocumentInformation.htm IfcDocumentInformation). }
  TIfcDocumentInformation = class(TIfcExternalInformation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionPort.htm IfcDistributionPort). }
  TIfcDistributionPort = class(TIfcPort)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionResource.htm IfcConstructionResource). }
  TIfcConstructionResource = class abstract(TIfcResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionBoardType.htm IfcDistributionBoardType). }
  TIfcDistributionBoardType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionBoard.htm IfcDistributionBoard). }
  TIfcDistributionBoard = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionResourceType.htm IfcConstructionResourceType). }
  TIfcConstructionResourceType = class abstract(TIfcTypeResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDamperType.htm IfcDamperType). }
  TIfcDamperType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDamper.htm IfcDamper). }
  TIfcDamper = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurveSegment.htm IfcCurveSegment). }
  TIfcCurveSegment = class(TIfcSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcController.htm IfcController). }
  TIfcController = class(TIfcDistributionControlElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConveyorSegment.htm IfcConveyorSegment). }
  TIfcConveyorSegment = class(TIfcFlowSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConveyorSegmentType.htm IfcConveyorSegmentType). }
  TIfcConveyorSegmentType = class(TIfcFlowSegmentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCosineSpiral.htm IfcCosineSpiral). }
  TIfcCosineSpiral = class(TIfcSpiral)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCostValue.htm IfcCostValue). }
  TIfcCostValue = class(TIfcAppliedValue)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCostSchedule.htm IfcCostSchedule). }
  TIfcCostSchedule = class(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCostItem.htm IfcCostItem). }
  TIfcCostItem = class(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConnectedFaceSet.htm IfcConnectedFaceSet). }
  TIfcConnectedFaceSet = class(TIfcTopologicalRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDuctSegmentType.htm IfcDuctSegmentType). }
  TIfcDuctSegmentType = class(TIfcFlowSegmentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableFitting.htm IfcCableFitting). }
  TIfcCableFitting = class(TIfcFlowFitting)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDuctSegment.htm IfcDuctSegment). }
  TIfcDuctSegment = class(TIfcFlowSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableFittingType.htm IfcCableFittingType). }
  TIfcCableFittingType = class(TIfcFlowFittingType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableSegment.htm IfcCableSegment). }
  TIfcCableSegment = class(TIfcFlowSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCableSegmentType.htm IfcCableSegmentType). }
  TIfcCableSegmentType = class(TIfcFlowSegmentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCaissonFoundation.htm IfcCaissonFoundation). }
  TIfcCaissonFoundation = class(TIfcDeepFoundation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCaissonFoundationType.htm IfcCaissonFoundationType). }
  TIfcCaissonFoundationType = class(TIfcDeepFoundationType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCenterLineProfileDef.htm IfcCenterLineProfileDef). }
  TIfcCenterLineProfileDef = class(TIfcArbitraryOpenProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDuctFittingType.htm IfcDuctFittingType). }
  TIfcDuctFittingType = class(TIfcFlowFittingType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCircleHollowProfileDef.htm IfcCircleHollowProfileDef). }
  TIfcCircleHollowProfileDef = class(TIfcCircleProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcClassification.htm IfcClassification). }
  TIfcClassification = class(TIfcExternalInformation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcClothoid.htm IfcClothoid). }
  TIfcClothoid = class(TIfcSpiral)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDuctFitting.htm IfcDuctFitting). }
  TIfcDuctFitting = class(TIfcFlowFitting)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcColourRgb.htm IfcColourRgb). }
  TIfcColourRgb = class(TIfcColourSpecification)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCompositeCurveOnSurface.htm IfcCompositeCurveOnSurface). }
  TIfcCompositeCurveOnSurface = class(TIfcCompositeCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCompositeCurveSegment.htm IfcCompositeCurveSegment). }
  TIfcCompositeCurveSegment = class(TIfcSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCompressor.htm IfcCompressor). }
  TIfcCompressor = class(TIfcFlowMovingDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCompressorType.htm IfcCompressorType). }
  TIfcCompressorType = class(TIfcFlowMovingDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFlowMeterType.htm IfcFlowMeterType). }
  TIfcFlowMeterType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignment.htm IfcAlignment). }
  TIfcAlignment = class(TIfcLinearPositioningElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentCant.htm IfcAlignmentCant). }
  TIfcAlignmentCant = class(TIfcLinearElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlanarBox.htm IfcPlanarBox). }
  TIfcPlanarBox = class(TIfcPlanarExtent)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPipeSegmentType.htm IfcPipeSegmentType). }
  TIfcPipeSegmentType = class(TIfcFlowSegmentType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentCantSegment.htm IfcAlignmentCantSegment). }
  TIfcAlignmentCantSegment = class(TIfcAlignmentParameterSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPipeSegment.htm IfcPipeSegment). }
  TIfcPipeSegment = class(TIfcFlowSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPipeFittingType.htm IfcPipeFittingType). }
  TIfcPipeFittingType = class(TIfcFlowFittingType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPipeFitting.htm IfcPipeFitting). }
  TIfcPipeFitting = class(TIfcFlowFitting)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPileType.htm IfcPileType). }
  TIfcPileType = class(TIfcDeepFoundationType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPile.htm IfcPile). }
  TIfcPile = class(TIfcDeepFoundation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPhysicalSimpleQuantity.htm IfcPhysicalSimpleQuantity). }
  TIfcPhysicalSimpleQuantity = class abstract(TIfcPhysicalQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPhysicalComplexQuantity.htm IfcPhysicalComplexQuantity). }
  TIfcPhysicalComplexQuantity = class(TIfcPhysicalQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPermit.htm IfcPermit). }
  TIfcPermit = class(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPermeableCoveringProperties.htm IfcPermeableCoveringProperties). }
  TIfcPermeableCoveringProperties = class(TIfcPreDefinedPropertySet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPerformanceHistory.htm IfcPerformanceHistory). }
  TIfcPerformanceHistory = class(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPath.htm IfcPath). }
  TIfcPath = class(TIfcTopologicalRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentHorizontal.htm IfcAlignmentHorizontal). }
  TIfcAlignmentHorizontal = class(TIfcLinearElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentHorizontalSegment.htm IfcAlignmentHorizontalSegment). }
  TIfcAlignmentHorizontalSegment = class(TIfcAlignmentParameterSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentSegment.htm IfcAlignmentSegment). }
  TIfcAlignmentSegment = class(TIfcLinearElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentVertical.htm IfcAlignmentVertical). }
  TIfcAlignmentVertical = class(TIfcLinearElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcActionRequest.htm IfcActionRequest). }
  TIfcActionRequest = class(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcActuator.htm IfcActuator). }
  TIfcActuator = class(TIfcDistributionControlElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAdvancedBrep.htm IfcAdvancedBrep). }
  TIfcAdvancedBrep = class(TIfcManifoldSolidBrep)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPumpType.htm IfcPumpType). }
  TIfcPumpType = class(TIfcFlowMovingDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPump.htm IfcPump). }
  TIfcPump = class(TIfcFlowMovingDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProtectiveDeviceType.htm IfcProtectiveDeviceType). }
  TIfcProtectiveDeviceType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAirTerminalBox.htm IfcAirTerminalBox). }
  TIfcAirTerminalBox = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAirTerminalBoxType.htm IfcAirTerminalBoxType). }
  TIfcAirTerminalBoxType = class(TIfcFlowControllerType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProtectiveDeviceTrippingUnit.htm IfcProtectiveDeviceTrippingUnit). }
  TIfcProtectiveDeviceTrippingUnit = class(TIfcDistributionControlElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProtectiveDevice.htm IfcProtectiveDevice). }
  TIfcProtectiveDevice = class(TIfcFlowController)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlarm.htm IfcAlarm). }
  TIfcAlarm = class(TIfcDistributionControlElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProjectionElement.htm IfcProjectionElement). }
  TIfcProjectionElement = class(TIfcFeatureElementAddition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProjectedCRS.htm IfcProjectedCRS). }
  TIfcProjectedCRS = class(TIfcCoordinateReferenceSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProjectOrder.htm IfcProjectOrder). }
  TIfcProjectOrder = class(TIfcControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProcedureType.htm IfcProcedureType). }
  TIfcProcedureType = class(TIfcTypeProcess)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcProcedure.htm IfcProcedure). }
  TIfcProcedure = class(TIfcProcess)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPreDefinedTextFont.htm IfcPreDefinedTextFont). }
  TIfcPreDefinedTextFont = class abstract(TIfcPreDefinedItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPreDefinedCurveFont.htm IfcPreDefinedCurveFont). }
  TIfcPreDefinedCurveFont = class abstract(TIfcPreDefinedItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPreDefinedColour.htm IfcPreDefinedColour). }
  TIfcPreDefinedColour = class abstract(TIfcPreDefinedItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPolygonalBoundedHalfSpace.htm IfcPolygonalBoundedHalfSpace). }
  TIfcPolygonalBoundedHalfSpace = class(TIfcHalfSpaceSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightSourceDirectional.htm IfcLightSourceDirectional). }
  TIfcLightSourceDirectional = class(TIfcLightSource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightSourceAmbient.htm IfcLightSourceAmbient). }
  TIfcLightSourceAmbient = class(TIfcLightSource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLibraryInformation.htm IfcLibraryInformation). }
  TIfcLibraryInformation = class(TIfcExternalInformation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLagTime.htm IfcLagTime). }
  TIfcLagTime = class(TIfcSchedulingTime)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAsset.htm IfcAsset). }
  TIfcAsset = class(TIfcGroup)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBSplineCurveWithKnots.htm IfcBSplineCurveWithKnots). }
  TIfcBSplineCurveWithKnots = class(TIfcBSplineCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcJunctionBoxType.htm IfcJunctionBoxType). }
  TIfcJunctionBoxType = class(TIfcFlowFittingType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcJunctionBox.htm IfcJunctionBox). }
  TIfcJunctionBox = class(TIfcFlowFitting)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIrregularTimeSeries.htm IfcIrregularTimeSeries). }
  TIfcIrregularTimeSeries = class(TIfcTimeSeries)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcInventory.htm IfcInventory). }
  TIfcInventory = class(TIfcGroup)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcIntersectionCurve.htm IfcIntersectionCurve). }
  TIfcIntersectionCurve = class(TIfcSurfaceCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcInterceptorType.htm IfcInterceptorType). }
  TIfcInterceptorType = class(TIfcFlowTreatmentDeviceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcInterceptor.htm IfcInterceptor). }
  TIfcInterceptor = class(TIfcFlowTreatmentDevice)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGradientCurve.htm IfcGradientCurve). }
  TIfcGradientCurve = class(TIfcCompositeCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeotechnicalStratum.htm IfcGeotechnicalStratum). }
  TIfcGeotechnicalStratum = class(TIfcGeotechnicalElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeotechnicalAssembly.htm IfcGeotechnicalAssembly). }
  TIfcGeotechnicalAssembly = class abstract(TIfcGeotechnicalElement)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBlock.htm IfcBlock). }
  TIfcBlock = class(TIfcCsgPrimitive3D)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeometricCurveSet.htm IfcGeometricCurveSet). }
  TIfcGeometricCurveSet = class(TIfcGeometricSet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeographicCRS.htm IfcGeographicCRS). }
  TIfcGeographicCRS = class(TIfcCoordinateReferenceSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightSourceGoniometric.htm IfcLightSourceGoniometric). }
  TIfcLightSourceGoniometric = class(TIfcLightSource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMetric.htm IfcMetric). }
  TIfcMetric = class(TIfcConstraint)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOffsetCurve2D.htm IfcOffsetCurve2D). }
  TIfcOffsetCurve2D = class(TIfcOffsetCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMarinePart.htm IfcMarinePart). }
  TIfcMarinePart = class(TIfcFacilityPart)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOccupant.htm IfcOccupant). }
  TIfcOccupant = class(TIfcActor)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcObjective.htm IfcObjective). }
  TIfcObjective = class(TIfcConstraint)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOffsetCurve3D.htm IfcOffsetCurve3D). }
  TIfcOffsetCurve3D = class(TIfcOffsetCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOffsetCurveByDistances.htm IfcOffsetCurveByDistances). }
  TIfcOffsetCurveByDistances = class(TIfcOffsetCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMapConversion.htm IfcMapConversion). }
  TIfcMapConversion = class(TIfcCoordinateOperation)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLoop.htm IfcLoop). }
  TIfcLoop = class(TIfcTopologicalRepresentationItem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAlignmentVerticalSegment.htm IfcAlignmentVerticalSegment). }
  TIfcAlignmentVerticalSegment = class(TIfcAlignmentParameterSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightSourcePositional.htm IfcLightSourcePositional). }
  TIfcLightSourcePositional = class(TIfcLightSource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMirroredProfileDef.htm IfcMirroredProfileDef). }
  TIfcMirroredProfileDef = class(TIfcDerivedProfileDef)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuildingSystem.htm IfcBuildingSystem). }
  TIfcBuildingSystem = class(TIfcSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTaskTimeRecurring.htm IfcTaskTimeRecurring). }
  TIfcTaskTimeRecurring = class(TIfcTaskTime)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBSplineSurface.htm IfcBSplineSurface). }
  TIfcBSplineSurface = class abstract(TIfcBoundedSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAdvancedBrepWithVoids.htm IfcAdvancedBrepWithVoids). }
  TIfcAdvancedBrepWithVoids = class(TIfcAdvancedBrep)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBuiltSystem.htm IfcBuiltSystem). }
  TIfcBuiltSystem = class(TIfcSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWorkPlan.htm IfcWorkPlan). }
  TIfcWorkPlan = class(TIfcWorkControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcWorkSchedule.htm IfcWorkSchedule). }
  TIfcWorkSchedule = class(TIfcWorkControl)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcZone.htm IfcZone). }
  TIfcZone = class(TIfcSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcClosedShell.htm IfcClosedShell). }
  TIfcClosedShell = class(TIfcConnectedFaceSet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVertexLoop.htm IfcVertexLoop). }
  TIfcVertexLoop = class(TIfcLoop)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcTextStyleFontModel.htm IfcTextStyleFontModel). }
  TIfcTextStyleFontModel = class(TIfcPreDefinedTextFont)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcVertexPoint.htm IfcVertexPoint). }
  TIfcVertexPoint = class(TIfcVertex)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcToroidalSurface.htm IfcToroidalSurface). }
  TIfcToroidalSurface = class(TIfcElementarySurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionEquipmentResource.htm IfcConstructionEquipmentResource). }
  TIfcConstructionEquipmentResource = class(TIfcConstructionResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionEquipmentResourceType.htm IfcConstructionEquipmentResourceType). }
  TIfcConstructionEquipmentResourceType = class(TIfcConstructionResourceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoundaryCurve.htm IfcBoundaryCurve). }
  TIfcBoundaryCurve = class(TIfcCompositeCurveOnSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionMaterialResource.htm IfcConstructionMaterialResource). }
  TIfcConstructionMaterialResource = class(TIfcConstructionResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBoundaryNodeConditionWarping.htm IfcBoundaryNodeConditionWarping). }
  TIfcBoundaryNodeConditionWarping = class(TIfcBoundaryNodeCondition)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionMaterialResourceType.htm IfcConstructionMaterialResourceType). }
  TIfcConstructionMaterialResourceType = class(TIfcConstructionResourceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionProductResource.htm IfcConstructionProductResource). }
  TIfcConstructionProductResource = class(TIfcConstructionResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcConstructionProductResourceType.htm IfcConstructionProductResourceType). }
  TIfcConstructionProductResourceType = class(TIfcConstructionResourceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBorehole.htm IfcBorehole). }
  TIfcBorehole = class(TIfcGeotechnicalAssembly)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLaborResourceType.htm IfcLaborResourceType). }
  TIfcLaborResourceType = class(TIfcConstructionResourceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLaborResource.htm IfcLaborResource). }
  TIfcLaborResource = class(TIfcConstructionResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeoslice.htm IfcGeoslice). }
  TIfcGeoslice = class(TIfcGeotechnicalAssembly)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcGeomodel.htm IfcGeomodel). }
  TIfcGeomodel = class(TIfcGeotechnicalAssembly)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFacetedBrepWithVoids.htm IfcFacetedBrepWithVoids). }
  TIfcFacetedBrepWithVoids = class(TIfcFacetedBrep)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFaceSurface.htm IfcFaceSurface). }
  TIfcFaceSurface = class(TIfcFace)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcFaceOuterBound.htm IfcFaceOuterBound). }
  TIfcFaceOuterBound = class(TIfcFaceBound)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSpaceType.htm IfcSpaceType). }
  TIfcSpaceType = class(TIfcSpatialStructureElementType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSphericalSurface.htm IfcSphericalSurface). }
  TIfcSphericalSurface = class(TIfcElementarySurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEdgeLoop.htm IfcEdgeLoop). }
  TIfcEdgeLoop = class(TIfcLoop)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralAnalysisModel.htm IfcStructuralAnalysisModel). }
  TIfcStructuralAnalysisModel = class(TIfcSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcEdgeCurve.htm IfcEdgeCurve). }
  TIfcEdgeCurve = class(TIfcEdge)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralCurveAction.htm IfcStructuralCurveAction). }
  TIfcStructuralCurveAction = class(TIfcStructuralAction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralCurveConnection.htm IfcStructuralCurveConnection). }
  TIfcStructuralCurveConnection = class(TIfcStructuralConnection)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralCurveMember.htm IfcStructuralCurveMember). }
  TIfcStructuralCurveMember = class(TIfcStructuralMember)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralCurveReaction.htm IfcStructuralCurveReaction). }
  TIfcStructuralCurveReaction = class(TIfcStructuralReaction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcLightSourceSpot.htm IfcLightSourceSpot). }
  TIfcLightSourceSpot = class(TIfcLightSourcePositional)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantityWeight.htm IfcQuantityWeight). }
  TIfcQuantityWeight = class(TIfcPhysicalSimpleQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantityVolume.htm IfcQuantityVolume). }
  TIfcQuantityVolume = class(TIfcPhysicalSimpleQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantityTime.htm IfcQuantityTime). }
  TIfcQuantityTime = class(TIfcPhysicalSimpleQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantityNumber.htm IfcQuantityNumber). }
  TIfcQuantityNumber = class(TIfcPhysicalSimpleQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantityLength.htm IfcQuantityLength). }
  TIfcQuantityLength = class(TIfcPhysicalSimpleQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantityCount.htm IfcQuantityCount). }
  TIfcQuantityCount = class(TIfcPhysicalSimpleQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcQuantityArea.htm IfcQuantityArea). }
  TIfcQuantityArea = class(TIfcPhysicalSimpleQuantity)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRationalBSplineCurveWithKnots.htm IfcRationalBSplineCurveWithKnots). }
  TIfcRationalBSplineCurveWithKnots = class(TIfcBSplineCurveWithKnots)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRectangularTrimmedSurface.htm IfcRectangularTrimmedSurface). }
  TIfcRectangularTrimmedSurface = class(TIfcBoundedSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPolyLoop.htm IfcPolyLoop). }
  TIfcPolyLoop = class(TIfcLoop)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcPlane.htm IfcPlane). }
  TIfcPlane = class(TIfcElementarySurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRelAssignsToGroupByFactor.htm IfcRelAssignsToGroupByFactor). }
  TIfcRelAssignsToGroupByFactor = class(TIfcRelAssignsToGroup)
  strict private
  end;

  { IFC internal class IfcOrientedEdge-temp (not in specification, only in XSD). }
  TIfcOrientedEdge_Temp = class abstract(TIfcEdge)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOpenShell.htm IfcOpenShell). }
  TIfcOpenShell = class(TIfcConnectedFaceSet)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcReparametrisedCompositeCurveSegment.htm IfcReparametrisedCompositeCurveSegment). }
  TIfcReparametrisedCompositeCurveSegment = class(TIfcCompositeCurveSegment)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcMapConversionScaled.htm IfcMapConversionScaled). }
  TIfcMapConversionScaled = class(TIfcMapConversion)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadCase.htm IfcStructuralLoadCase). }
  TIfcStructuralLoadCase = class(TIfcStructuralLoadGroup)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralSurfaceConnection.htm IfcStructuralSurfaceConnection). }
  TIfcStructuralSurfaceConnection = class(TIfcStructuralConnection)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralSurfaceMember.htm IfcStructuralSurfaceMember). }
  TIfcStructuralSurfaceMember = class(TIfcStructuralMember)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralSurfaceReaction.htm IfcStructuralSurfaceReaction). }
  TIfcStructuralSurfaceReaction = class(TIfcStructuralReaction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDraughtingPreDefinedColour.htm IfcDraughtingPreDefinedColour). }
  TIfcDraughtingPreDefinedColour = class(TIfcPreDefinedColour)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSubContractResource.htm IfcSubContractResource). }
  TIfcSubContractResource = class(TIfcConstructionResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSubContractResourceType.htm IfcSubContractResourceType). }
  TIfcSubContractResourceType = class(TIfcConstructionResourceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSubedge.htm IfcSubedge). }
  TIfcSubedge = class(TIfcEdge)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionSystem.htm IfcDistributionSystem). }
  TIfcDistributionSystem = class(TIfcSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceOfLinearExtrusion.htm IfcSurfaceOfLinearExtrusion). }
  TIfcSurfaceOfLinearExtrusion = class(TIfcSweptSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceOfRevolution.htm IfcSurfaceOfRevolution). }
  TIfcSurfaceOfRevolution = class(TIfcSweptSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcSurfaceReinforcementArea.htm IfcSurfaceReinforcementArea). }
  TIfcSurfaceReinforcementArea = class(TIfcStructuralLoadOrResult)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDirectrixDerivedReferenceSweptAreaSolid.htm IfcDirectrixDerivedReferenceSweptAreaSolid). }
  TIfcDirectrixDerivedReferenceSweptAreaSolid = class(TIfcFixedReferenceSweptAreaSolid)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCylindricalSurface.htm IfcCylindricalSurface). }
  TIfcCylindricalSurface = class(TIfcElementarySurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurveBoundedSurface.htm IfcCurveBoundedSurface). }
  TIfcCurveBoundedSurface = class(TIfcBoundedSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCurveBoundedPlane.htm IfcCurveBoundedPlane). }
  TIfcCurveBoundedPlane = class(TIfcBoundedSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCrewResourceType.htm IfcCrewResourceType). }
  TIfcCrewResourceType = class(TIfcConstructionResourceType)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcCrewResource.htm IfcCrewResource). }
  TIfcCrewResource = class(TIfcConstructionResource)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadStatic.htm IfcStructuralLoadStatic). }
  TIfcStructuralLoadStatic = class abstract(TIfcStructuralLoadOrResult)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralPointAction.htm IfcStructuralPointAction). }
  TIfcStructuralPointAction = class(TIfcStructuralAction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralPointConnection.htm IfcStructuralPointConnection). }
  TIfcStructuralPointConnection = class(TIfcStructuralConnection)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralPointReaction.htm IfcStructuralPointReaction). }
  TIfcStructuralPointReaction = class(TIfcStructuralReaction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDraughtingPreDefinedCurveFont.htm IfcDraughtingPreDefinedCurveFont). }
  TIfcDraughtingPreDefinedCurveFont = class(TIfcPreDefinedCurveFont)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralSurfaceAction.htm IfcStructuralSurfaceAction). }
  TIfcStructuralSurfaceAction = class(TIfcStructuralAction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLinearAction.htm IfcStructuralLinearAction). }
  TIfcStructuralLinearAction = class(TIfcStructuralCurveAction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadLinearForce.htm IfcStructuralLoadLinearForce). }
  TIfcStructuralLoadLinearForce = class(TIfcStructuralLoadStatic)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralCurveMemberVarying.htm IfcStructuralCurveMemberVarying). }
  TIfcStructuralCurveMemberVarying = class(TIfcStructuralCurveMember)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadPlanarForce.htm IfcStructuralLoadPlanarForce). }
  TIfcStructuralLoadPlanarForce = class(TIfcStructuralLoadStatic)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadSingleDisplacement.htm IfcStructuralLoadSingleDisplacement). }
  TIfcStructuralLoadSingleDisplacement = class(TIfcStructuralLoadStatic)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcDistributionCircuit.htm IfcDistributionCircuit). }
  TIfcDistributionCircuit = class(TIfcDistributionSystem)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcAdvancedFace.htm IfcAdvancedFace). }
  TIfcAdvancedFace = class(TIfcFaceSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadSingleForce.htm IfcStructuralLoadSingleForce). }
  TIfcStructuralLoadSingleForce = class(TIfcStructuralLoadStatic)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadTemperature.htm IfcStructuralLoadTemperature). }
  TIfcStructuralLoadTemperature = class(TIfcStructuralLoadStatic)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralPlanarAction.htm IfcStructuralPlanarAction). }
  TIfcStructuralPlanarAction = class(TIfcStructuralSurfaceAction)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOuterBoundaryCurve.htm IfcOuterBoundaryCurve). }
  TIfcOuterBoundaryCurve = class(TIfcBoundaryCurve)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcOrientedEdge.htm IfcOrientedEdge). }
  TIfcOrientedEdge = class(TIfcOrientedEdge_Temp)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcBSplineSurfaceWithKnots.htm IfcBSplineSurfaceWithKnots). }
  TIfcBSplineSurfaceWithKnots = class(TIfcBSplineSurface)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralSurfaceMemberVarying.htm IfcStructuralSurfaceMemberVarying). }
  TIfcStructuralSurfaceMemberVarying = class(TIfcStructuralSurfaceMember)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcRationalBSplineSurfaceWithKnots.htm IfcRationalBSplineSurfaceWithKnots). }
  TIfcRationalBSplineSurfaceWithKnots = class(TIfcBSplineSurfaceWithKnots)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadSingleForceWarping.htm IfcStructuralLoadSingleForceWarping). }
  TIfcStructuralLoadSingleForceWarping = class(TIfcStructuralLoadSingleForce)
  strict private
  end;

  { IFC class @url(https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/lexical/IfcStructuralLoadSingleDisplacementDistortion.htm IfcStructuralLoadSingleDisplacementDistortion). }
  TIfcStructuralLoadSingleDisplacementDistortion = class(TIfcStructuralLoadSingleDisplacement)
  strict private
  end;

{$endif read_interface_types}

{$ifdef read_implementation}

{$endif read_implementation}

{$ifdef read_add_classes}
  FIfcClasses.Add(TIfcSegment);
  FIfcClasses.Add(TIfcGeometricRepresentationSubContext_Temp);
  FIfcClasses.Add(TIfcGeometricSet);
  FIfcClasses.Add(TIfcSectionedSpine);
  FIfcClasses.Add(TIfcGeotechnicalElement);
  FIfcClasses.Add(TIfcSectionedSolid);
  FIfcClasses.Add(TIfcGridPlacement);
  FIfcClasses.Add(TIfcGroup);
  FIfcClasses.Add(TIfcHalfSpaceSolid);
  FIfcClasses.Add(TIfcHeatExchanger);
  FIfcClasses.Add(TIfcHeatExchangerType);
  FIfcClasses.Add(TIfcHumidifier);
  FIfcClasses.Add(TIfcFlowMovingDevice);
  FIfcClasses.Add(TIfcFlowMovingDeviceType);
  FIfcClasses.Add(TIfcFlowSegment);
  FIfcClasses.Add(TIfcFlowSegmentType);
  FIfcClasses.Add(TIfcFlowStorageDevice);
  FIfcClasses.Add(TIfcFlowStorageDeviceType);
  FIfcClasses.Add(TIfcFlowTreatmentDevice);
  FIfcClasses.Add(TIfcFlowTreatmentDeviceType);
  FIfcClasses.Add(TIfcFooting);
  FIfcClasses.Add(TIfcFootingType);
  FIfcClasses.Add(TIfcGeographicElement);
  FIfcClasses.Add(TIfcGeographicElementType);
  FIfcClasses.Add(TIfcKerb);
  FIfcClasses.Add(TIfcKerbType);
  FIfcClasses.Add(TIfcLShapeProfileDef);
  FIfcClasses.Add(TIfcRoof);
  FIfcClasses.Add(TIfcRoad);
  FIfcClasses.Add(TIfcLamp);
  FIfcClasses.Add(TIfcLampType);
  FIfcClasses.Add(TIfcLibraryReference);
  FIfcClasses.Add(TIfcLightDistributionData);
  FIfcClasses.Add(TIfcLightFixture);
  FIfcClasses.Add(TIfcLightFixtureType);
  FIfcClasses.Add(TIfcLightIntensityDistribution);
  FIfcClasses.Add(TIfcHumidifierType);
  FIfcClasses.Add(TIfcIShapeProfileDef);
  FIfcClasses.Add(TIfcImpactProtectionDevice);
  FIfcClasses.Add(TIfcImpactProtectionDeviceType);
  FIfcClasses.Add(TIfcIndexedColourMap);
  FIfcClasses.Add(TIfcIndexedPolygonalFaceWithVoids);
  FIfcClasses.Add(TIfcIndexedTriangleTextureMap);
  FIfcClasses.Add(TIfcSchedulingTime);
  FIfcClasses.Add(TIfcSIUnit_Temp);
  FIfcClasses.Add(TIfcIrregularTimeSeriesValue);
  FIfcClasses.Add(TIfcRoundedRectangleProfileDef);
  FIfcClasses.Add(TIfcRoofType);
  FIfcClasses.Add(TIfcSpatialElementType);
  FIfcClasses.Add(TIfcSpaceHeaterType);
  FIfcClasses.Add(TIfcExternalInformation);
  FIfcClasses.Add(TIfcSpaceHeater);
  FIfcClasses.Add(TIfcExternalSpatialStructureElement);
  FIfcClasses.Add(TIfcExternallyDefinedHatchStyle);
  FIfcClasses.Add(TIfcExternallyDefinedSurfaceStyle);
  FIfcClasses.Add(TIfcExternallyDefinedTextFont);
  FIfcClasses.Add(TIfcExtrudedAreaSolidTapered);
  FIfcClasses.Add(TIfcSpace);
  FIfcClasses.Add(TIfcFaceBasedSurfaceModel);
  FIfcClasses.Add(TIfcSolarDeviceType);
  FIfcClasses.Add(TIfcElectricGeneratorType);
  FIfcClasses.Add(TIfcElectricMotor);
  FIfcClasses.Add(TIfcElectricMotorType);
  FIfcClasses.Add(TIfcStackTerminal);
  FIfcClasses.Add(TIfcSpiral);
  FIfcClasses.Add(TIfcEllipseProfileDef);
  FIfcClasses.Add(TIfcEngine);
  FIfcClasses.Add(TIfcEngineType);
  FIfcClasses.Add(TIfcEvaporativeCooler);
  FIfcClasses.Add(TIfcEvaporativeCoolerType);
  FIfcClasses.Add(TIfcEvaporator);
  FIfcClasses.Add(TIfcEvaporatorType);
  FIfcClasses.Add(TIfcFillAreaStyleTiles);
  FIfcClasses.Add(TIfcShellBasedSurfaceModel);
  FIfcClasses.Add(TIfcShadingDeviceType);
  FIfcClasses.Add(TIfcFireSuppressionTerminal);
  FIfcClasses.Add(TIfcFireSuppressionTerminalType);
  FIfcClasses.Add(TIfcShadingDevice);
  FIfcClasses.Add(TIfcFlowController);
  FIfcClasses.Add(TIfcFlowControllerType);
  FIfcClasses.Add(TIfcFlowFitting);
  FIfcClasses.Add(TIfcFlowFittingType);
  FIfcClasses.Add(TIfcFlowInstrumentType);
  FIfcClasses.Add(TIfcSensorType);
  FIfcClasses.Add(TIfcSolarDevice);
  FIfcClasses.Add(TIfcSimplePropertyTemplate);
  FIfcClasses.Add(TIfcFacilityPart);
  FIfcClasses.Add(TIfcSignalType);
  FIfcClasses.Add(TIfcSignal);
  FIfcClasses.Add(TIfcSignType);
  FIfcClasses.Add(TIfcSign);
  FIfcClasses.Add(TIfcFastener);
  FIfcClasses.Add(TIfcFastenerType);
  FIfcClasses.Add(TIfcFeatureElementAddition);
  FIfcClasses.Add(TIfcFillAreaStyle);
  FIfcClasses.Add(TIfcFillAreaStyleHatching);
  FIfcClasses.Add(TIfcPlate);
  FIfcClasses.Add(TIfcPlateType);
  FIfcClasses.Add(TIfcPointByDistanceExpression);
  FIfcClasses.Add(TIfcPointOnCurve);
  FIfcClasses.Add(TIfcPointOnSurface);
  FIfcClasses.Add(TIfcRelAssigns);
  FIfcClasses.Add(TIfcReinforcingMeshType);
  FIfcClasses.Add(TIfcPolynomialCurve);
  FIfcClasses.Add(TIfcPort);
  FIfcClasses.Add(TIfcReinforcingMesh);
  FIfcClasses.Add(TIfcPreDefinedItem);
  FIfcClasses.Add(TIfcPreDefinedProperties);
  FIfcClasses.Add(TIfcPavementType);
  FIfcClasses.Add(TIfcPcurve);
  FIfcClasses.Add(TIfcRelConnectsPathElements);
  FIfcClasses.Add(TIfcRelAssociatesProfileDef);
  FIfcClasses.Add(TIfcRelAssociatesLibrary);
  FIfcClasses.Add(TIfcRelAssociatesDocument);
  FIfcClasses.Add(TIfcPhysicalQuantity);
  FIfcClasses.Add(TIfcRelAssociatesConstraint);
  FIfcClasses.Add(TIfcRelAssociatesClassification);
  FIfcClasses.Add(TIfcRelAssociatesApproval);
  FIfcClasses.Add(TIfcPixelTexture);
  FIfcClasses.Add(TIfcPlanarExtent);
  FIfcClasses.Add(TIfcRectangleHollowProfileDef);
  FIfcClasses.Add(TIfcProtectiveDeviceTrippingUnitType);
  FIfcClasses.Add(TIfcRampType);
  FIfcClasses.Add(TIfcRampFlightType);
  FIfcClasses.Add(TIfcRampFlight);
  FIfcClasses.Add(TIfcRamp);
  FIfcClasses.Add(TIfcRailway);
  FIfcClasses.Add(TIfcRailingType);
  FIfcClasses.Add(TIfcQuantitySet);
  FIfcClasses.Add(TIfcRailing);
  FIfcClasses.Add(TIfcRailType);
  FIfcClasses.Add(TIfcRail);
  FIfcClasses.Add(TIfcPreDefinedPropertySet);
  FIfcClasses.Add(TIfcPresentationLayerWithStyle);
  FIfcClasses.Add(TIfcProcess);
  FIfcClasses.Add(TIfcReferent);
  FIfcClasses.Add(TIfcReference);
  FIfcClasses.Add(TIfcRecurrencePattern);
  FIfcClasses.Add(TIfcPropertyBoundedValue);
  FIfcClasses.Add(TIfcPropertyEnumeratedValue);
  FIfcClasses.Add(TIfcPropertyEnumeration);
  FIfcClasses.Add(TIfcPropertyListValue);
  FIfcClasses.Add(TIfcPropertyReferenceValue);
  FIfcClasses.Add(TIfcPropertyTableValue);
  FIfcClasses.Add(TIfcMarineFacility);
  FIfcClasses.Add(TIfcMaterialClassificationRelationship);
  FIfcClasses.Add(TIfcMaterialLayerWithOffsets);
  FIfcClasses.Add(TIfcMaterialList_Spec);
  FIfcClasses.Add(TIfcMaterialProfileSetUsageTapering);
  FIfcClasses.Add(TIfcMaterialProfileWithOffsets);
  FIfcClasses.Add(TIfcMechanicalFastener);
  FIfcClasses.Add(TIfcMechanicalFastenerType);
  FIfcClasses.Add(TIfcMedicalDevice);
  FIfcClasses.Add(TIfcMedicalDeviceType);
  FIfcClasses.Add(TIfcMember);
  FIfcClasses.Add(TIfcMemberType);
  FIfcClasses.Add(TIfcLightSource);
  FIfcClasses.Add(TIfcRevolvedAreaSolid);
  FIfcClasses.Add(TIfcLine);
  FIfcClasses.Add(TIfcLinearElement);
  FIfcClasses.Add(TIfcLinearPlacement);
  FIfcClasses.Add(TIfcLinearPositioningElement);
  FIfcClasses.Add(TIfcLiquidTerminal);
  FIfcClasses.Add(TIfcLiquidTerminalType);
  FIfcClasses.Add(TIfcResourceConstraintRelationship);
  FIfcClasses.Add(TIfcManifoldSolidBrep);
  FIfcClasses.Add(TIfcResourceApprovalRelationship);
  FIfcClasses.Add(TIfcResource);
  FIfcClasses.Add(TIfcOffsetCurve);
  FIfcClasses.Add(TIfcRelNests);
  FIfcClasses.Add(TIfcRelFlowControlElements);
  FIfcClasses.Add(TIfcRelCoversSpaces);
  FIfcClasses.Add(TIfcOpenCrossProfileDef);
  FIfcClasses.Add(TIfcRelConnectsStructuralMember);
  FIfcClasses.Add(TIfcRelConnectsStructuralActivity);
  FIfcClasses.Add(TIfcRelConnectsPorts);
  FIfcClasses.Add(TIfcOutlet);
  FIfcClasses.Add(TIfcOutletType);
  FIfcClasses.Add(TIfcRelConnectsPortToElement);
  FIfcClasses.Add(TIfcPavement);
  FIfcClasses.Add(TIfcRelSpaceBoundary1stLevel);
  FIfcClasses.Add(TIfcRelServicesBuildings);
  FIfcClasses.Add(TIfcMobileTelecommunicationsAppliance);
  FIfcClasses.Add(TIfcMobileTelecommunicationsApplianceType);
  FIfcClasses.Add(TIfcMooringDevice);
  FIfcClasses.Add(TIfcMooringDeviceType);
  FIfcClasses.Add(TIfcMotorConnection);
  FIfcClasses.Add(TIfcMotorConnectionType);
  FIfcClasses.Add(TIfcNavigationElement);
  FIfcClasses.Add(TIfcNavigationElementType);
  FIfcClasses.Add(TIfcRelSequence);
  FIfcClasses.Add(TIfcRelReferencedInSpatialStructure);
  FIfcClasses.Add(TIfcTimeSeries);
  FIfcClasses.Add(TIfcCircleProfileDef);
  FIfcClasses.Add(TIfcCivilElement);
  FIfcClasses.Add(TIfcCivilElementType);
  FIfcClasses.Add(TIfcTimePeriod);
  FIfcClasses.Add(TIfcClassificationReference);
  FIfcClasses.Add(TIfcTextureVertex);
  FIfcClasses.Add(TIfcCoil);
  FIfcClasses.Add(TIfcCoilType);
  FIfcClasses.Add(TIfcTextureMap);
  FIfcClasses.Add(TIfcColourRgbList_Spec);
  FIfcClasses.Add(TIfcChimneyType);
  FIfcClasses.Add(TIfcTransformerType);
  FIfcClasses.Add(TIfcTransformer);
  FIfcClasses.Add(TIfcTrackElementType);
  FIfcClasses.Add(TIfcTrackElement);
  FIfcClasses.Add(TIfcTopologyRepresentation);
  FIfcClasses.Add(TIfcTopologicalRepresentationItem);
  FIfcClasses.Add(TIfcCartesianTransformationOperator2DnonUniform);
  FIfcClasses.Add(TIfcTimeSeriesValue);
  FIfcClasses.Add(TIfcChiller);
  FIfcClasses.Add(TIfcChillerType);
  FIfcClasses.Add(TIfcChimney);
  FIfcClasses.Add(TIfcColourSpecification);
  FIfcClasses.Add(TIfcConnectionCurveGeometry);
  FIfcClasses.Add(TIfcConnectionPointGeometry);
  FIfcClasses.Add(TIfcConnectionSurfaceGeometry);
  FIfcClasses.Add(TIfcConnectionVolumeGeometry);
  FIfcClasses.Add(TIfcConstraint);
  FIfcClasses.Add(TIfcTextLiteral);
  FIfcClasses.Add(TIfcTendonType);
  FIfcClasses.Add(TIfcTendonConduitType);
  FIfcClasses.Add(TIfcTendonConduit);
  FIfcClasses.Add(TIfcTendonAnchorType);
  FIfcClasses.Add(TIfcTendonAnchor);
  FIfcClasses.Add(TIfcTextStyle);
  FIfcClasses.Add(TIfcColumnType);
  FIfcClasses.Add(TIfcCommunicationsAppliance);
  FIfcClasses.Add(TIfcCommunicationsApplianceType);
  FIfcClasses.Add(TIfcComplexPropertyTemplate);
  FIfcClasses.Add(TIfcCompositeCurve);
  FIfcClasses.Add(TIfcTextureCoordinateIndicesWithVoids);
  FIfcClasses.Add(TIfcTextureCoordinateGenerator);
  FIfcClasses.Add(TIfcTextStyleTextModel);
  FIfcClasses.Add(TIfcTextStyleForDefinedFont);
  FIfcClasses.Add(TIfcCondenser);
  FIfcClasses.Add(TIfcCondenserType);
  FIfcClasses.Add(TIfcTransportationDevice);
  FIfcClasses.Add(TIfcAlignmentParameterSegment);
  FIfcClasses.Add(TIfcVibrationDamperType);
  FIfcClasses.Add(TIfcVibrationDamper);
  FIfcClasses.Add(TIfcAnnotation);
  FIfcClasses.Add(TIfcAnnotationFillArea);
  FIfcClasses.Add(TIfcAppliedValue);
  FIfcClasses.Add(TIfcApproval);
  FIfcClasses.Add(TIfcApprovalRelationship);
  FIfcClasses.Add(TIfcArbitraryOpenProfileDef);
  FIfcClasses.Add(TIfcAsymmetricIShapeProfileDef);
  FIfcClasses.Add(TIfcAudioVisualAppliance);
  FIfcClasses.Add(TIfcVibrationIsolator);
  FIfcClasses.Add(TIfcZShapeProfileDef);
  FIfcClasses.Add(TIfcActor);
  FIfcClasses.Add(TIfcWellKnownText);
  FIfcClasses.Add(TIfcAirToAirHeatRecovery);
  FIfcClasses.Add(TIfcAirToAirHeatRecoveryType);
  FIfcClasses.Add(TIfcWasteTerminalType);
  FIfcClasses.Add(TIfcAlarmType);
  FIfcClasses.Add(TIfcWasteTerminal);
  FIfcClasses.Add(TIfcVoidingFeature);
  FIfcClasses.Add(TIfcVirtualElement);
  FIfcClasses.Add(TIfcVibrationIsolatorType);
  FIfcClasses.Add(TIfcAudioVisualApplianceType);
  FIfcClasses.Add(TIfcTypeResource);
  FIfcClasses.Add(TIfcTypeProcess);
  FIfcClasses.Add(TIfcTubeBundleType);
  FIfcClasses.Add(TIfcBridge);
  FIfcClasses.Add(TIfcTubeBundle);
  FIfcClasses.Add(TIfcTriangulatedIrregularNetwork);
  FIfcClasses.Add(TIfcTrapeziumProfileDef);
  FIfcClasses.Add(TIfcBurner);
  FIfcClasses.Add(TIfcBurnerType);
  FIfcClasses.Add(TIfcCShapeProfileDef);
  FIfcClasses.Add(TIfcTransportationDeviceType);
  FIfcClasses.Add(TIfcUShapeProfileDef);
  FIfcClasses.Add(TIfcAxis1Placement);
  FIfcClasses.Add(TIfcAxis2PlacementLinear);
  FIfcClasses.Add(TIfcBSplineCurve);
  FIfcClasses.Add(TIfcVector);
  FIfcClasses.Add(TIfcBearing);
  FIfcClasses.Add(TIfcBearingType);
  FIfcClasses.Add(TIfcBlobTexture);
  FIfcClasses.Add(TIfcUnitaryEquipmentType);
  FIfcClasses.Add(TIfcBoundaryCondition);
  FIfcClasses.Add(TIfcUnitaryEquipment);
  FIfcClasses.Add(TIfcUnitaryControlElementType);
  FIfcClasses.Add(TIfcTendon);
  FIfcClasses.Add(TIfcSurfaceStyle);
  FIfcClasses.Add(TIfcDistributionChamberElementType);
  FIfcClasses.Add(TIfcDistributionChamberElement);
  FIfcClasses.Add(TIfcSurfaceStyleLighting);
  FIfcClasses.Add(TIfcSurfaceStyleRefraction);
  FIfcClasses.Add(TIfcDiscreteAccessoryType);
  FIfcClasses.Add(TIfcDiscreteAccessory);
  FIfcClasses.Add(TIfcDirectrixCurveSweptAreaSolid);
  FIfcClasses.Add(TIfcDerivedProfileDef);
  FIfcClasses.Add(TIfcDeepFoundationType);
  FIfcClasses.Add(TIfcDeepFoundation);
  FIfcClasses.Add(TIfcSurfaceStyleShading);
  FIfcClasses.Add(TIfcSweptDiskSolid);
  FIfcClasses.Add(TIfcCurveStyleFontPattern);
  FIfcClasses.Add(TIfcCurveStyleFontAndScaling);
  FIfcClasses.Add(TIfcCurveStyleFont);
  FIfcClasses.Add(TIfcCurveStyle);
  FIfcClasses.Add(TIfcCurtainWallType);
  FIfcClasses.Add(TIfcCurtainWall);
  FIfcClasses.Add(TIfcCurrencyRelationship);
  FIfcClasses.Add(TIfcDistributionControlElement);
  FIfcClasses.Add(TIfcElectricGenerator);
  FIfcClasses.Add(TIfcStackTerminalType);
  FIfcClasses.Add(TIfcStair);
  FIfcClasses.Add(TIfcStairFlight);
  FIfcClasses.Add(TIfcStairFlightType);
  FIfcClasses.Add(TIfcStairType);
  FIfcClasses.Add(TIfcElectricApplianceType);
  FIfcClasses.Add(TIfcElectricAppliance);
  FIfcClasses.Add(TIfcStructuralActivity);
  FIfcClasses.Add(TIfcStructuralConnectionCondition);
  FIfcClasses.Add(TIfcStructuralItem);
  FIfcClasses.Add(TIfcEarthworksElement);
  FIfcClasses.Add(TIfcEarthworksCut);
  FIfcClasses.Add(TIfcStructuralLoad);
  FIfcClasses.Add(TIfcStyleModel);
  FIfcClasses.Add(TIfcSurface);
  FIfcClasses.Add(TIfcDocumentReference);
  FIfcClasses.Add(TIfcDocumentInformationRelationship);
  FIfcClasses.Add(TIfcSurfaceCurve);
  FIfcClasses.Add(TIfcSurfaceFeature);
  FIfcClasses.Add(TIfcCovering);
  FIfcClasses.Add(TIfcCourseType);
  FIfcClasses.Add(TIfcCourse);
  FIfcClasses.Add(TIfcControl);
  FIfcClasses.Add(TIfcTable);
  FIfcClasses.Add(TIfcTableColumn);
  FIfcClasses.Add(TIfcTableRow);
  FIfcClasses.Add(TIfcCoordinateReferenceSystem);
  FIfcClasses.Add(TIfcControllerType);
  FIfcClasses.Add(TIfcCoordinateOperation);
  FIfcClasses.Add(TIfcCoolingTowerType);
  FIfcClasses.Add(TIfcCooledBeam);
  FIfcClasses.Add(TIfcCoolingTower);
  FIfcClasses.Add(TIfcCooledBeamType);
  FIfcClasses.Add(TIfcCsgSolid);
  FIfcClasses.Add(TIfcTShapeProfileDef);
  FIfcClasses.Add(TIfcCsgPrimitive3D);
  FIfcClasses.Add(TIfcContextDependentUnit);
  FIfcClasses.Add(TIfcCoveringType);
  FIfcClasses.Add(TIfcTextLiteralWithExtent);
  FIfcClasses.Add(TIfcReinforcementBarProperties);
  FIfcClasses.Add(TIfcTankType);
  FIfcClasses.Add(TIfcReinforcedSoil);
  FIfcClasses.Add(TIfcRegularTimeSeries);
  FIfcClasses.Add(TIfcRigidOperation);
  FIfcClasses.Add(TIfcVertex);
  FIfcClasses.Add(TIfcTaskType);
  FIfcClasses.Add(TIfcStructuralLoadConfiguration);
  FIfcClasses.Add(TIfcReinforcementDefinitionProperties);
  FIfcClasses.Add(TIfcStructuralLoadGroup);
  FIfcClasses.Add(TIfcVehicleType);
  FIfcClasses.Add(TIfcStructuralLoadOrResult);
  FIfcClasses.Add(TIfcStructuralMember);
  FIfcClasses.Add(TIfcStructuralReaction);
  FIfcClasses.Add(TIfcSecondOrderPolynomialSpiral);
  FIfcClasses.Add(TIfcSectionProperties);
  FIfcClasses.Add(TIfcSphere);
  FIfcClasses.Add(TIfcSeamCurve);
  FIfcClasses.Add(TIfcSpatialZoneType);
  FIfcClasses.Add(TIfcWindowLiningProperties);
  FIfcClasses.Add(TIfcRectangularPyramid);
  FIfcClasses.Add(TIfcStructuralAction);
  FIfcClasses.Add(TIfcRailwayPart);
  FIfcClasses.Add(TIfcRoadPart);
  FIfcClasses.Add(TIfcWindowPanelProperties);
  FIfcClasses.Add(TIfcTaskTime);
  FIfcClasses.Add(TIfcWorkCalendar);
  FIfcClasses.Add(TIfcWorkControl);
  FIfcClasses.Add(TIfcWorkTime);
  FIfcClasses.Add(TIfcStructuralConnection);
  FIfcClasses.Add(TIfcTask);
  FIfcClasses.Add(TIfcSpatialStructureElementType);
  FIfcClasses.Add(TIfcTank);
  FIfcClasses.Add(TIfcSectionReinforcementProperties);
  FIfcClasses.Add(TIfcSurfaceStyleRendering);
  FIfcClasses.Add(TIfcRelConnectsWithEccentricity);
  FIfcClasses.Add(TIfcSectionedSolidHorizontal);
  FIfcClasses.Add(TIfcSectionedSurface);
  FIfcClasses.Add(TIfcTransportElementType);
  FIfcClasses.Add(TIfcTransportElement);
  FIfcClasses.Add(TIfcRelSpaceBoundary2ndLevel);
  FIfcClasses.Add(TIfcSweptDiskSolidPolygonal);
  FIfcClasses.Add(TIfcSeventhOrderPolynomialSpiral);
  FIfcClasses.Add(TIfcSensor);
  FIfcClasses.Add(TIfcSystem);
  FIfcClasses.Add(TIfcThirdOrderPolynomialSpiral);
  FIfcClasses.Add(TIfcSweptSurface);
  FIfcClasses.Add(TIfcSwitchingDevice);
  FIfcClasses.Add(TIfcSwitchingDeviceType);
  FIfcClasses.Add(TIfcSegmentedReferenceCurve);
  FIfcClasses.Add(TIfcResourceTime);
  FIfcClasses.Add(TIfcStructuralResultGroup);
  FIfcClasses.Add(TIfcStyledRepresentation);
  FIfcClasses.Add(TIfcVehicle);
  FIfcClasses.Add(TIfcRelAssignsToActor);
  FIfcClasses.Add(TIfcValveType);
  FIfcClasses.Add(TIfcRelAssignsToControl);
  FIfcClasses.Add(TIfcSlippageConnectionCondition);
  FIfcClasses.Add(TIfcRelAssignsToGroup);
  FIfcClasses.Add(TIfcRelAssignsToProcess);
  FIfcClasses.Add(TIfcRelAssignsToProduct);
  FIfcClasses.Add(TIfcRelAssignsToResource);
  FIfcClasses.Add(TIfcSineSpiral);
  FIfcClasses.Add(TIfcValve);
  FIfcClasses.Add(TIfcRightCircularCylinder);
  FIfcClasses.Add(TIfcRightCircularCone);
  FIfcClasses.Add(TIfcSurfaceCurveSweptAreaSolid);
  FIfcClasses.Add(TIfcRevolvedAreaSolidTapered);
  FIfcClasses.Add(TIfcUnitaryControlElement);
  FIfcClasses.Add(TIfcBoundaryNodeCondition);
  FIfcClasses.Add(TIfcElectricTimeControlType);
  FIfcClasses.Add(TIfcElectricTimeControl);
  FIfcClasses.Add(TIfcElectricFlowTreatmentDeviceType);
  FIfcClasses.Add(TIfcElectricFlowTreatmentDevice);
  FIfcClasses.Add(TIfcElectricFlowStorageDeviceType);
  FIfcClasses.Add(TIfcElectricFlowStorageDevice);
  FIfcClasses.Add(TIfcElectricDistributionBoardType);
  FIfcClasses.Add(TIfcElectricDistributionBoard);
  FIfcClasses.Add(TIfcBoundedSurface);
  FIfcClasses.Add(TIfcBoxedHalfSpace);
  FIfcClasses.Add(TIfcEdge);
  FIfcClasses.Add(TIfcBridgePart);
  FIfcClasses.Add(TIfcCableCarrierFitting);
  FIfcClasses.Add(TIfcCableCarrierFittingType);
  FIfcClasses.Add(TIfcEarthworksFill);
  FIfcClasses.Add(TIfcCableCarrierSegment);
  FIfcClasses.Add(TIfcDuctSilencerType);
  FIfcClasses.Add(TIfcCableCarrierSegmentType);
  FIfcClasses.Add(TIfcElementQuantity);
  FIfcClasses.Add(TIfcFlowMeter);
  FIfcClasses.Add(TIfcFlowInstrument);
  FIfcClasses.Add(TIfcFixedReferenceSweptAreaSolid);
  FIfcClasses.Add(TIfcFilterType);
  FIfcClasses.Add(TIfcFilter);
  FIfcClasses.Add(TIfcFanType);
  FIfcClasses.Add(TIfcFan);
  FIfcClasses.Add(TIfcFailureConnectionCondition);
  FIfcClasses.Add(TIfcFacilityPartCommon);
  FIfcClasses.Add(TIfcFacetedBrep);
  FIfcClasses.Add(TIfcBoundaryEdgeCondition);
  FIfcClasses.Add(TIfcFaceBound);
  FIfcClasses.Add(TIfcFace);
  FIfcClasses.Add(TIfcExternalSpatialElement);
  FIfcClasses.Add(TIfcEventType);
  FIfcClasses.Add(TIfcBoundaryFaceCondition);
  FIfcClasses.Add(TIfcEventTime);
  FIfcClasses.Add(TIfcEvent);
  FIfcClasses.Add(TIfcElementarySurface);
  FIfcClasses.Add(TIfcDuctSilencer);
  FIfcClasses.Add(TIfcDoorPanelProperties);
  FIfcClasses.Add(TIfcConnectionPointEccentricity);
  FIfcClasses.Add(TIfcDoorLiningProperties);
  FIfcClasses.Add(TIfcDocumentInformation);
  FIfcClasses.Add(TIfcDistributionPort);
  FIfcClasses.Add(TIfcConstructionResource);
  FIfcClasses.Add(TIfcDistributionBoardType);
  FIfcClasses.Add(TIfcDistributionBoard);
  FIfcClasses.Add(TIfcConstructionResourceType);
  FIfcClasses.Add(TIfcDamperType);
  FIfcClasses.Add(TIfcDamper);
  FIfcClasses.Add(TIfcCurveSegment);
  FIfcClasses.Add(TIfcController);
  FIfcClasses.Add(TIfcConveyorSegment);
  FIfcClasses.Add(TIfcConveyorSegmentType);
  FIfcClasses.Add(TIfcCosineSpiral);
  FIfcClasses.Add(TIfcCostValue);
  FIfcClasses.Add(TIfcCostSchedule);
  FIfcClasses.Add(TIfcCostItem);
  FIfcClasses.Add(TIfcConnectedFaceSet);
  FIfcClasses.Add(TIfcDuctSegmentType);
  FIfcClasses.Add(TIfcCableFitting);
  FIfcClasses.Add(TIfcDuctSegment);
  FIfcClasses.Add(TIfcCableFittingType);
  FIfcClasses.Add(TIfcCableSegment);
  FIfcClasses.Add(TIfcCableSegmentType);
  FIfcClasses.Add(TIfcCaissonFoundation);
  FIfcClasses.Add(TIfcCaissonFoundationType);
  FIfcClasses.Add(TIfcCenterLineProfileDef);
  FIfcClasses.Add(TIfcDuctFittingType);
  FIfcClasses.Add(TIfcCircleHollowProfileDef);
  FIfcClasses.Add(TIfcClassification);
  FIfcClasses.Add(TIfcClothoid);
  FIfcClasses.Add(TIfcDuctFitting);
  FIfcClasses.Add(TIfcColourRgb);
  FIfcClasses.Add(TIfcCompositeCurveOnSurface);
  FIfcClasses.Add(TIfcCompositeCurveSegment);
  FIfcClasses.Add(TIfcCompressor);
  FIfcClasses.Add(TIfcCompressorType);
  FIfcClasses.Add(TIfcFlowMeterType);
  FIfcClasses.Add(TIfcAlignment);
  FIfcClasses.Add(TIfcAlignmentCant);
  FIfcClasses.Add(TIfcPlanarBox);
  FIfcClasses.Add(TIfcPipeSegmentType);
  FIfcClasses.Add(TIfcAlignmentCantSegment);
  FIfcClasses.Add(TIfcPipeSegment);
  FIfcClasses.Add(TIfcPipeFittingType);
  FIfcClasses.Add(TIfcPipeFitting);
  FIfcClasses.Add(TIfcPileType);
  FIfcClasses.Add(TIfcPile);
  FIfcClasses.Add(TIfcPhysicalSimpleQuantity);
  FIfcClasses.Add(TIfcPhysicalComplexQuantity);
  FIfcClasses.Add(TIfcPermit);
  FIfcClasses.Add(TIfcPermeableCoveringProperties);
  FIfcClasses.Add(TIfcPerformanceHistory);
  FIfcClasses.Add(TIfcPath);
  FIfcClasses.Add(TIfcAlignmentHorizontal);
  FIfcClasses.Add(TIfcAlignmentHorizontalSegment);
  FIfcClasses.Add(TIfcAlignmentSegment);
  FIfcClasses.Add(TIfcAlignmentVertical);
  FIfcClasses.Add(TIfcActionRequest);
  FIfcClasses.Add(TIfcActuator);
  FIfcClasses.Add(TIfcAdvancedBrep);
  FIfcClasses.Add(TIfcPumpType);
  FIfcClasses.Add(TIfcPump);
  FIfcClasses.Add(TIfcProtectiveDeviceType);
  FIfcClasses.Add(TIfcAirTerminalBox);
  FIfcClasses.Add(TIfcAirTerminalBoxType);
  FIfcClasses.Add(TIfcProtectiveDeviceTrippingUnit);
  FIfcClasses.Add(TIfcProtectiveDevice);
  FIfcClasses.Add(TIfcAlarm);
  FIfcClasses.Add(TIfcProjectionElement);
  FIfcClasses.Add(TIfcProjectedCRS);
  FIfcClasses.Add(TIfcProjectOrder);
  FIfcClasses.Add(TIfcProcedureType);
  FIfcClasses.Add(TIfcProcedure);
  FIfcClasses.Add(TIfcPreDefinedTextFont);
  FIfcClasses.Add(TIfcPreDefinedCurveFont);
  FIfcClasses.Add(TIfcPreDefinedColour);
  FIfcClasses.Add(TIfcPolygonalBoundedHalfSpace);
  FIfcClasses.Add(TIfcLightSourceDirectional);
  FIfcClasses.Add(TIfcLightSourceAmbient);
  FIfcClasses.Add(TIfcLibraryInformation);
  FIfcClasses.Add(TIfcLagTime);
  FIfcClasses.Add(TIfcAsset);
  FIfcClasses.Add(TIfcBSplineCurveWithKnots);
  FIfcClasses.Add(TIfcJunctionBoxType);
  FIfcClasses.Add(TIfcJunctionBox);
  FIfcClasses.Add(TIfcIrregularTimeSeries);
  FIfcClasses.Add(TIfcInventory);
  FIfcClasses.Add(TIfcIntersectionCurve);
  FIfcClasses.Add(TIfcInterceptorType);
  FIfcClasses.Add(TIfcInterceptor);
  FIfcClasses.Add(TIfcGradientCurve);
  FIfcClasses.Add(TIfcGeotechnicalStratum);
  FIfcClasses.Add(TIfcGeotechnicalAssembly);
  FIfcClasses.Add(TIfcBlock);
  FIfcClasses.Add(TIfcGeometricCurveSet);
  FIfcClasses.Add(TIfcGeographicCRS);
  FIfcClasses.Add(TIfcLightSourceGoniometric);
  FIfcClasses.Add(TIfcMetric);
  FIfcClasses.Add(TIfcOffsetCurve2D);
  FIfcClasses.Add(TIfcMarinePart);
  FIfcClasses.Add(TIfcOccupant);
  FIfcClasses.Add(TIfcObjective);
  FIfcClasses.Add(TIfcOffsetCurve3D);
  FIfcClasses.Add(TIfcOffsetCurveByDistances);
  FIfcClasses.Add(TIfcMapConversion);
  FIfcClasses.Add(TIfcLoop);
  FIfcClasses.Add(TIfcAlignmentVerticalSegment);
  FIfcClasses.Add(TIfcLightSourcePositional);
  FIfcClasses.Add(TIfcMirroredProfileDef);
  FIfcClasses.Add(TIfcBuildingSystem);
  FIfcClasses.Add(TIfcTaskTimeRecurring);
  FIfcClasses.Add(TIfcBSplineSurface);
  FIfcClasses.Add(TIfcAdvancedBrepWithVoids);
  FIfcClasses.Add(TIfcBuiltSystem);
  FIfcClasses.Add(TIfcWorkPlan);
  FIfcClasses.Add(TIfcWorkSchedule);
  FIfcClasses.Add(TIfcZone);
  FIfcClasses.Add(TIfcClosedShell);
  FIfcClasses.Add(TIfcVertexLoop);
  FIfcClasses.Add(TIfcTextStyleFontModel);
  FIfcClasses.Add(TIfcVertexPoint);
  FIfcClasses.Add(TIfcToroidalSurface);
  FIfcClasses.Add(TIfcConstructionEquipmentResource);
  FIfcClasses.Add(TIfcConstructionEquipmentResourceType);
  FIfcClasses.Add(TIfcBoundaryCurve);
  FIfcClasses.Add(TIfcConstructionMaterialResource);
  FIfcClasses.Add(TIfcBoundaryNodeConditionWarping);
  FIfcClasses.Add(TIfcConstructionMaterialResourceType);
  FIfcClasses.Add(TIfcConstructionProductResource);
  FIfcClasses.Add(TIfcConstructionProductResourceType);
  FIfcClasses.Add(TIfcBorehole);
  FIfcClasses.Add(TIfcLaborResourceType);
  FIfcClasses.Add(TIfcLaborResource);
  FIfcClasses.Add(TIfcGeoslice);
  FIfcClasses.Add(TIfcGeomodel);
  FIfcClasses.Add(TIfcFacetedBrepWithVoids);
  FIfcClasses.Add(TIfcFaceSurface);
  FIfcClasses.Add(TIfcFaceOuterBound);
  FIfcClasses.Add(TIfcSpaceType);
  FIfcClasses.Add(TIfcSphericalSurface);
  FIfcClasses.Add(TIfcEdgeLoop);
  FIfcClasses.Add(TIfcStructuralAnalysisModel);
  FIfcClasses.Add(TIfcEdgeCurve);
  FIfcClasses.Add(TIfcStructuralCurveAction);
  FIfcClasses.Add(TIfcStructuralCurveConnection);
  FIfcClasses.Add(TIfcStructuralCurveMember);
  FIfcClasses.Add(TIfcStructuralCurveReaction);
  FIfcClasses.Add(TIfcLightSourceSpot);
  FIfcClasses.Add(TIfcQuantityWeight);
  FIfcClasses.Add(TIfcQuantityVolume);
  FIfcClasses.Add(TIfcQuantityTime);
  FIfcClasses.Add(TIfcQuantityNumber);
  FIfcClasses.Add(TIfcQuantityLength);
  FIfcClasses.Add(TIfcQuantityCount);
  FIfcClasses.Add(TIfcQuantityArea);
  FIfcClasses.Add(TIfcRationalBSplineCurveWithKnots);
  FIfcClasses.Add(TIfcRectangularTrimmedSurface);
  FIfcClasses.Add(TIfcPolyLoop);
  FIfcClasses.Add(TIfcPlane);
  FIfcClasses.Add(TIfcRelAssignsToGroupByFactor);
  FIfcClasses.Add(TIfcOrientedEdge_Temp);
  FIfcClasses.Add(TIfcOpenShell);
  FIfcClasses.Add(TIfcReparametrisedCompositeCurveSegment);
  FIfcClasses.Add(TIfcMapConversionScaled);
  FIfcClasses.Add(TIfcStructuralLoadCase);
  FIfcClasses.Add(TIfcStructuralSurfaceConnection);
  FIfcClasses.Add(TIfcStructuralSurfaceMember);
  FIfcClasses.Add(TIfcStructuralSurfaceReaction);
  FIfcClasses.Add(TIfcDraughtingPreDefinedColour);
  FIfcClasses.Add(TIfcSubContractResource);
  FIfcClasses.Add(TIfcSubContractResourceType);
  FIfcClasses.Add(TIfcSubedge);
  FIfcClasses.Add(TIfcDistributionSystem);
  FIfcClasses.Add(TIfcSurfaceOfLinearExtrusion);
  FIfcClasses.Add(TIfcSurfaceOfRevolution);
  FIfcClasses.Add(TIfcSurfaceReinforcementArea);
  FIfcClasses.Add(TIfcDirectrixDerivedReferenceSweptAreaSolid);
  FIfcClasses.Add(TIfcCylindricalSurface);
  FIfcClasses.Add(TIfcCurveBoundedSurface);
  FIfcClasses.Add(TIfcCurveBoundedPlane);
  FIfcClasses.Add(TIfcCrewResourceType);
  FIfcClasses.Add(TIfcCrewResource);
  FIfcClasses.Add(TIfcStructuralLoadStatic);
  FIfcClasses.Add(TIfcStructuralPointAction);
  FIfcClasses.Add(TIfcStructuralPointConnection);
  FIfcClasses.Add(TIfcStructuralPointReaction);
  FIfcClasses.Add(TIfcDraughtingPreDefinedCurveFont);
  FIfcClasses.Add(TIfcStructuralSurfaceAction);
  FIfcClasses.Add(TIfcStructuralLinearAction);
  FIfcClasses.Add(TIfcStructuralLoadLinearForce);
  FIfcClasses.Add(TIfcStructuralCurveMemberVarying);
  FIfcClasses.Add(TIfcStructuralLoadPlanarForce);
  FIfcClasses.Add(TIfcStructuralLoadSingleDisplacement);
  FIfcClasses.Add(TIfcDistributionCircuit);
  FIfcClasses.Add(TIfcAdvancedFace);
  FIfcClasses.Add(TIfcStructuralLoadSingleForce);
  FIfcClasses.Add(TIfcStructuralLoadTemperature);
  FIfcClasses.Add(TIfcStructuralPlanarAction);
  FIfcClasses.Add(TIfcOuterBoundaryCurve);
  FIfcClasses.Add(TIfcOrientedEdge);
  FIfcClasses.Add(TIfcBSplineSurfaceWithKnots);
  FIfcClasses.Add(TIfcStructuralSurfaceMemberVarying);
  FIfcClasses.Add(TIfcRationalBSplineSurfaceWithKnots);
  FIfcClasses.Add(TIfcStructuralLoadSingleForceWarping);
  FIfcClasses.Add(TIfcStructuralLoadSingleDisplacementDistortion);

{$endif read_add_classes}