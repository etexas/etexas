/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.webapp.ejb;

import java.awt.Polygon;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import org.etexascode.devicedata.AppInitConfig;
import org.etexascode.devicedata.DualIntIdentifier;
import org.etexascode.devicedata.FixedCellDeviceData;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IConnectedVehicleApp;
import org.etexascode.devicedata.IDeviceData;
import org.etexascode.devicedata.NativeApp;
import org.etexascode.devicedata.RSEDeviceData;
import org.etexascode.devicedata.ReportDeviceData;
import org.etexascode.interrep.datamodel.Detector;
import org.etexascode.interrep.datamodel.ExecMetaData;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.LaneNode;
import org.etexascode.interrep.datamodel.ReferencePoint;
import org.etexascode.interrep.datamodel.SimulatorInterface;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsLatLongConversion;
import org.etexascode.interrep.topography.DSRCTopography;
import org.etexascode.interrep.topography.ITopography;
import org.etexascode.interrep.topography.ITopographyFeature;
import org.etexascode.interrep.topography.TopographyPolygon;
import org.etexascode.wavesim.CellTower;
import org.etexascode.wavesim.CellularConfig;
import org.etexascode.wavesim.Constants;
import org.etexascode.webapp.datamodel.CellularConfiguration;
import org.etexascode.webapp.datamodel.Composite;
import org.etexascode.webapp.datamodel.LaneMapping;
import org.etexascode.webapp.datamodel.Simulation;
import org.etexascode.webapp.datamodel.application.Application;
import org.etexascode.webapp.datamodel.device.CellularDeviceProfile;
import org.etexascode.webapp.datamodel.device.Device;
import org.etexascode.webapp.datamodel.device.DeviceProfile;
import org.etexascode.webapp.datamodel.device.FixedCellularDevice;
import org.etexascode.webapp.datamodel.device.ObuDeviceProfile;
import org.etexascode.webapp.datamodel.device.ReportDevice;
import org.etexascode.webapp.datamodel.device.RseDevice;
import org.etexascode.webapp.datamodel.topography.Building;
import org.etexascode.webapp.datamodel.topography.TopographyFeature;
import org.etexascode.webapp.datamodel.topography.TopographyType;
import org.etexascode.webapp.exception.WebAppException;
import org.etexascode.webapp.ra.api.SimFactory;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException;
import org.etexascode.webapp.ra.api.exceptions.simresource.SimResourceException.SimResourceErrorType;
import org.slf4j.LoggerFactory;

import com.harmonia.etexas.wave.NativeAppManager;

/**
 * Provides utility methods to build an execution.
 * 
 * @author bbadillo
 * @author ablatt
 * @author jrutherford
 * @author dranker
 * @author ttevendale
 * @author emyers
 */
class ExecutionBuilder {

    /** The random seed number generator. */
    private static final Random SEED_GENERATOR = new Random();

    /* prevents instantiation */
    private ExecutionBuilder() {}

    /**
     * Builds the execution meta data for the specified composite.
     * 
     * @param composite The composite to be executed.
     * @return The map of execution meta data for the specified composite.
     */
    static Map<Long, ExecMetaData> buildExecMetaData(Composite composite) {

        Map<Long, ExecMetaData> metaDataMap = new HashMap<Long, ExecMetaData>();

        for (Simulation simulation : composite.getSimulations()) {

            List<Detector> detectors = new ArrayList<Detector>();
            for (org.etexascode.webapp.datamodel.Detector detectorModel : simulation.getDetectors()) {
                detectors.add(ExecutionBuilder.convertToDetector(detectorModel, simulation.getLaneManagerData().getLaneManager()));
            }

            ExecMetaData metaData = new ExecMetaData();
            metaData.setSimulationId(simulation.getId());
            metaData.setExecDetectors(detectors);
            metaData.setGeoCalculatorType(composite.getGeographicCalculator());
            metaData.setPropagationLossModel(composite.getPropagationLossModel());
            double[] simulationLatLon = UtilsLatLongConversion.convertCentimeterOffsetToLatLong(simulation.getX(), simulation.getY(), composite.getLatitude(), composite.getLongitude(),
                    composite.getGeographicCalculator());
            metaData.setReferencePoints(new ReferencePoint[] { new ReferencePoint(simulationLatLon[0], simulationLatLon[1]) });
            metaData.setX(simulation.getX());
            metaData.setY(simulation.getY());

            Map<DualIntIdentifier, DualIntIdentifier> laneMap = new HashMap<DualIntIdentifier, DualIntIdentifier>();

            for (LaneMapping laneMapping : composite.getLaneMappings()) {

                if (laneMapping.getSourceSimulation() == simulation.getId()) {

                    DualIntIdentifier source = new DualIntIdentifier((int)laneMapping.getSourceSimulation(), laneMapping.getSourceLane(), false);
                    DualIntIdentifier target = new DualIntIdentifier((int)laneMapping.getTargetSimulation(), laneMapping.getTargetLane(), false);
                    laneMap.put(source, target);
                }
            }

            metaData.setLaneMap(laneMap);
            metaDataMap.put(simulation.getId(), metaData);
        }

        return metaDataMap;
    }

    /**
     * Converts a webapp detector model into an interrep detector.
     * 
     * @param detectorModel The detector model to convert.
     * @param laneManager The lane manager to use for the conversion.
     * @return The converted detector model.
     */
    private static Detector convertToDetector(org.etexascode.webapp.datamodel.Detector detectorModel, LaneManager laneManager) {

        Detector detector = new Detector();
        detector.setDetectorID(detectorModel.getId().intValue());
        detector.setLaneIDs(Arrays.asList(detectorModel.getLane()));
        detector.setPresenceDetectCap(true);

        double totalDistance = 0.0;
        double distanceToStop = detectorModel.getDistance();
        List<LaneNode> nodes = laneManager.getLaneById(detectorModel.getLane()).getLaneGeomList();
        LaneNode currentNode = nodes.get(nodes.size() - 1);
        double width = detectorModel.getWidth();
        double height = detectorModel.getHeight();

        for (LaneNode node : nodes) {

            double distanceToNode = UtilsCalculations.getDistance(currentNode, node);
            totalDistance += distanceToNode;

            if (totalDistance >= distanceToStop) {

                double deltaX = currentNode.getX() - node.getX();
                double deltaY = currentNode.getY() - node.getY();
                double[] frontCenter = UtilsCalculations.moveAlongAxis(new double[] { currentNode.getX(), currentNode.getY() }, -deltaY, -deltaX,
                        distanceToStop - (totalDistance - distanceToNode));

                double[] frontLeft = UtilsCalculations.moveAlongAxis(frontCenter, deltaX, -deltaY, width / 2.0);
                double[] frontRight = UtilsCalculations.moveAlongAxis(frontCenter, -deltaX, deltaY, width / 2.0);
                double[] rearLeft = UtilsCalculations.moveAlongAxis(frontLeft, -deltaY, -deltaX, height);
                double[] rearRight = UtilsCalculations.moveAlongAxis(frontRight, -deltaY, -deltaX, height);

                Polygon area = detector.getArea();
                area.addPoint((int)Math.round(frontLeft[0]), (int)Math.round(frontLeft[1]));
                area.addPoint((int)Math.round(frontRight[0]), (int)Math.round(frontRight[1]));
                area.addPoint((int)Math.round(rearRight[0]), (int)Math.round(rearRight[1]));
                area.addPoint((int)Math.round(rearLeft[0]), (int)Math.round(rearLeft[1]));

                break;
            }

            currentNode = node;
        }

        return detector;
    }

    /**
     * Builds the cell towers for the specified composite.
     * 
     * @param composite The composite to be executed.
     * @return A list of cell towers for the specified composite.
     */
    static List<CellTower> buildCellTowers(Composite composite) {

        List<CellTower> cellTowers = new ArrayList<CellTower>();

        for (org.etexascode.webapp.datamodel.CellTower cellTowerModel : composite.getCellTowers()) {

            cellTowers.add(ExecutionBuilder.convertToCellTower(cellTowerModel));
        }

        return cellTowers;
    }

    /**
     * Converts a webapp cell tower model into a wavesim cell tower.
     * 
     * @param cellTowerModel The cell tower model to convert.
     * @return The converted cell tower model.
     */
    private static CellTower convertToCellTower(org.etexascode.webapp.datamodel.CellTower cellTowerModel) {

        CellTower cellTower = new CellTower();
        cellTower.setProvider(cellTowerModel.getProvider());
        cellTower.setX(cellTowerModel.getX());
        cellTower.setY(cellTowerModel.getY());
        cellTower.setZ(cellTowerModel.getZ());

        return cellTower;
    }

    /**
     * Builds the cellular configuration for the specified composite.
     * 
     * @param composite The composite to be executed.
     * @return The cellular configuration for the specified composite.
     */
    static CellularConfig buildCellularConfig(Composite composite) {

        CellularConfig configuration = new CellularConfig();
        CellularConfiguration configurationModel = composite.getCellularConfiguration();
        configuration.setCellNoiseFigure(configurationModel.getCellularDeviceNoise());
        configuration.setCellTowerNoiseFigure(configurationModel.getCellTowerNoise());
        configuration.setCellTowerTxPower(configurationModel.getCellTowerPower());
        configuration.setCellTxPower(configurationModel.getCellularDevicePower());
        configuration.setDownlinkBandwidth(configurationModel.getDownlinkBandwidth());
        configuration.setDownlinkCarrierFrequency(configurationModel.getDownlinkCarrierFrequency());
        configuration.setUplinkBandwidth(configurationModel.getUplinkBandwidth());
        configuration.setUplinkCarrierFrequency(configurationModel.getUplinkCarrierFrequency());

        return configuration;
    }

    /**
     * Builds the topography for the specified composite.
     * 
     * @param composite The composite to be executed.
     * @return The topography for the specified composite.
     */
    static ITopography buildTopography(Composite composite) {

        List<ITopographyFeature> topographyFeatures = new ArrayList<ITopographyFeature>();
        for (TopographyFeature featureModel : composite.getTopographyFeatures()) {
            topographyFeatures.add(ExecutionBuilder.convertToTopographyFeature(featureModel));
        }

        return new DSRCTopography(topographyFeatures);
    }

    /**
     * Converts a webapp topography feature model into an interrep topography feature.
     * 
     * @param featureModel The topography feature model to convert.
     * @return The converted topography feature model.
     */
    private static ITopographyFeature convertToTopographyFeature(TopographyFeature featureModel) {

        ITopographyFeature convertedFeature = null;

        if (TopographyType.BUILDING.equals(featureModel.getType())) {

            Building building = (Building)featureModel;

            double halfWidth = building.getWidth() * 0.5;
            double halfLength = building.getLength() * 0.5;

            List<Point2D> points = new ArrayList<Point2D>(4);
            points.add(new Point2D.Double(building.getX() + halfWidth, building.getY() + halfLength));
            points.add(new Point2D.Double(building.getX() + halfWidth, building.getY() - halfLength));
            points.add(new Point2D.Double(building.getX() - halfWidth, building.getY() - halfLength));
            points.add(new Point2D.Double(building.getX() - halfWidth, building.getY() + halfLength));

            convertedFeature = new TopographyPolygon(building.getId(), points, building.getHeight());
        }
        return convertedFeature;
    }

    /**
     * Builds the device data for the specified composite.
     * 
     * @param composite The composite to be executed.
     * @return A list of the device data for the specified composite.
     * @throws WebAppException If the device data cannot be built.
     */
    static List<IDeviceData> buildDeviceData(Composite composite) throws WebAppException {

        List<IDeviceData> deviceData = new ArrayList<IDeviceData>();
        deviceData.add(ExecutionBuilder.convertToDeviceData(composite.getReportDevice(), composite));

        for (Device deviceModel : composite.getDevices()) {

            if (deviceModel instanceof RseDevice) {

                deviceData.add(ExecutionBuilder.convertToDeviceData((RseDevice)deviceModel, composite));
            }
            else if (deviceModel instanceof FixedCellularDevice) {

                deviceData.add(ExecutionBuilder.convertToDeviceData((FixedCellularDevice)deviceModel));
            }
        }

        return deviceData;
    }

    /**
     * Converts the specified fixed cellular device model into fixed cell device data.
     * 
     * @param deviceModel The fixed cellular device model to convert.
     * @return The converted fixed cellular device model.
     * @throws WebAppException If the fixed cellular device model cannot be converted.
     */
    private static FixedCellDeviceData convertToDeviceData(FixedCellularDevice deviceModel) throws WebAppException {

        return new FixedCellDeviceData(deviceModel.getId(), ExecutionBuilder.parseApplications(deviceModel), deviceModel.getMacAddress(), deviceModel.getX(), deviceModel.getY(), deviceModel.getZ());
    }

    /**
     * Converts the specified report device model into report device data.
     * 
     * @param deviceModel The report device model to convert.
     * @param composite The parent composite.
     * @return The converted report device model.
     * @throws WebAppException If the report device model cannot be converted.
     */
    private static ReportDeviceData convertToDeviceData(ReportDevice deviceModel, Composite composite) throws WebAppException {

        int index = 0;
        int[] intersections = new int[composite.getSimulations().size()];
        for (Simulation simulation : composite.getSimulations()) {
            intersections[index++] = simulation.getId().intValue();
        }

        return new ReportDeviceData(ExecutionBuilder.parseApplications(deviceModel), intersections);
    }

    /**
     * Converts the specified RSE device model into RSE device data.
     * 
     * @param deviceModel The RSE device model to convert.
     * @param composite The parent composite.
     * @return The converted RSE device model.
     * @throws WebAppException If the RSE device model cannot be converted.
     */
    private static RSEDeviceData convertToDeviceData(RseDevice deviceModel, Composite composite) throws WebAppException {

        ReferencePoint[] referencePoints = new ReferencePoint[] { new ReferencePoint(composite.getLatitude(), composite.getLongitude()) };

        int index = 0;
        int[] intersections = new int[composite.getSimulations().size()];
        for (Simulation simulation : composite.getSimulations()) {
            intersections[index++] = simulation.getId().intValue();
        }

        return new RSEDeviceData(deviceModel.getId(), ExecutionBuilder.parseApplications(deviceModel), referencePoints, intersections, deviceModel.getX(), deviceModel.getY(),
                deviceModel.getZ());
    }

    /**
     * Parses a list of application instances for the specified device model.
     * 
     * @param deviceModel The host device model.
     * @return A list of hosted application instances.
     * @throws WebAppException If any application instance cannot be parsed.
     */
    private static List<IConnectedVehicleApp<?>> parseApplications(Device deviceModel) throws WebAppException {

        List<IConnectedVehicleApp<?>> applicationInstances = new ArrayList<IConnectedVehicleApp<?>>();

        for (Application applicationModel : deviceModel.getApplications()) {

            applicationInstances.add(ExecutionBuilder.parseApplication(applicationModel));
        }

        return applicationInstances;
    }

    /**
     * Parses an application instance from the specified application instance model.
     * 
     * @param applicationModel The application instance model to parse.
     * @return A parsed application instance.
     * @throws WebAppException If an application instance cannot be parsed.
     */
    private static IConnectedVehicleApp<?> parseApplication(Application applicationModel) throws WebAppException {

        IConnectedVehicleApp<?> applicationInstance;

        try {

            applicationInstance = (IConnectedVehicleApp<?>)applicationModel.getInstanceClass().newInstance();

            if (applicationInstance instanceof IAppLifecycle) {

                ((IAppLifecycle)applicationInstance).init(applicationModel.getInstanceParameters());
            }
        }
        catch (IllegalAccessException | InstantiationException | RuntimeException exception) {

            String exceptionTitle = "Parse Application Failure";
            LoggerFactory.getLogger(ExecutionBuilder.class).error(exceptionTitle, exception);
            throw new WebAppException(exceptionTitle, String.format("An instance of the \"%s\" application could not be created.", applicationModel.getName()));
        }

        return applicationInstance;
    }

    /**
     * Builds the cellular application configurations for the specified device profile models.
     * 
     * @param deviceProfileModels The list of device profile models.
     * @return A list of cellular application configurations.
     * @throws WebAppException If any cellular application configuration cannot be built.
     */
    static List<AppInitConfig> buildCellularApplicationConfigurations(List<DeviceProfile> deviceProfileModels) throws WebAppException {

        List<CellularDeviceProfile> cellularDeviceProfiles = new ArrayList<CellularDeviceProfile>();

        for (DeviceProfile deviceProfile : deviceProfileModels) {

            if (deviceProfile instanceof CellularDeviceProfile) {

                cellularDeviceProfiles.add((CellularDeviceProfile)deviceProfile);
            }
        }

        return ExecutionBuilder.parseApplicationConfigurations(cellularDeviceProfiles, 0, 999);
    }

    /**
     * Builds the DSRC application configurations for the specified device profile models.
     * 
     * @param deviceProfileModels The list of device profile models.
     * @return A list of DSRC application configurations.
     * @throws WebAppException If any DSRC application configurations cannot be built.
     */
    static List<AppInitConfig> buildDsrcApplicationConfigurations(List<DeviceProfile> deviceProfileModels) throws WebAppException {

        List<ObuDeviceProfile> obuDeviceProfiles = new ArrayList<ObuDeviceProfile>();

        for (DeviceProfile deviceProfile : deviceProfileModels) {

            if (deviceProfile instanceof ObuDeviceProfile) {

                obuDeviceProfiles.add((ObuDeviceProfile)deviceProfile);
            }
        }

        return ExecutionBuilder.parseApplicationConfigurations(obuDeviceProfiles, 0, 999);
    }

    /**
     * Parses a list of application configurations for the specified device profile models.
     * 
     * @param deviceProfileModels The list of device profile models.
     * @param lowerBound The integer lower bound to use.
     * @param upperBound The integer upper bound to use.
     * @return A list of parsed application configurations.
     * @throws WebAppException If any application configuration cannot be parsed.
     */
    private static List<AppInitConfig> parseApplicationConfigurations(List<? extends DeviceProfile> deviceProfileModels, int lowerBound, int upperBound) throws WebAppException {

        List<AppInitConfig> applicationConfigurations = new ArrayList<AppInitConfig>();

        for (DeviceProfile deviceProfileModel : deviceProfileModels) {

            int var = (int)((deviceProfileModel.getPercentage() / 100) * (upperBound - lowerBound));

            AppInitConfig applicationConfiguration = null;

            if (applicationConfigurations.isEmpty()) {

                applicationConfiguration = ExecutionBuilder.parseApplicationConfiguration(deviceProfileModel, lowerBound, lowerBound + var);
            }
            else {

                AppInitConfig lastApplicationConfiguration = applicationConfigurations.get(applicationConfigurations.size() - 1);

                if ((lowerBound + var) < lastApplicationConfiguration.max) {

                    applicationConfiguration = ExecutionBuilder.parseApplicationConfiguration(deviceProfileModel, lowerBound + var, lastApplicationConfiguration.max);
                }
            }

            if (applicationConfiguration != null) {

                for (AppInitConfig existingConfiguration : applicationConfigurations) {
                    existingConfiguration.appDefs.addAll(applicationConfiguration.appDefs);
                    existingConfiguration.configs.addAll(applicationConfiguration.configs);
                }

                applicationConfigurations.add(applicationConfiguration);
            }

            lowerBound += var + 1;
        }

        return applicationConfigurations;
    }

    /**
     * Parses an application configuration for the specified device profile model.
     * 
     * @param deviceProfileModel The device profile model.
     * @param lowerBound The integer lower bound to use.
     * @param upperBound The integer upper bound to use.
     * @return The parsed application configuration.
     * @throws WebAppException If an application configuration cannot be parsed.
     */
    private static AppInitConfig parseApplicationConfiguration(DeviceProfile deviceProfileModel, int lowerBound, int upperBound) throws WebAppException {

        List<Class<?>> instanceClasses = new ArrayList<Class<?>>();
        List<String[]> instanceParameters = new ArrayList<String[]>();

        for (Application applicationModel : deviceProfileModel.getApplications()) {

            try {

                instanceClasses.add(applicationModel.getInstanceClass());
                instanceParameters.add(applicationModel.getInstanceParameters());
            }
            catch (RuntimeException exception) {

                String exceptionTitle = "Parse Application Configuration Failure";
                LoggerFactory.getLogger(ExecutionBuilder.class).error(exceptionTitle, exception);
                throw new WebAppException(exceptionTitle, String.format("A configuration for the \"%s\" application could not be created.", applicationModel.getName()));
            }
        }

        int minDevices = (deviceProfileModel instanceof CellularDeviceProfile) ? ((CellularDeviceProfile)deviceProfileModel).getMinDevices() : 1;
        int maxDevices = (deviceProfileModel instanceof CellularDeviceProfile) ? ((CellularDeviceProfile)deviceProfileModel).getMaxDevices() : 1;

        return new AppInitConfig(deviceProfileModel.getId(), instanceClasses, instanceParameters, lowerBound, upperBound, minDevices, maxDevices);
    }

    /**
     * Builds the native application manager for the specified device data.
     * 
     * @param executionId The long ID of the execution.
     * @param deviceData The list of execution device data.
     * @return The native application manager for the specified device data.
     */
    static NativeAppManager buildNativeApplicationManager(Long executionId, List<IDeviceData> deviceData) {

        List<NativeApp<?>> nativeApplications = new ArrayList<NativeApp<?>>();

        for (IDeviceData device : deviceData) {

            for (IConnectedVehicleApp<?> application : device.getApps()) {

                if (application instanceof NativeApp<?>) {

                    nativeApplications.add((NativeApp<?>)application);
                }
            }
        }

        NativeAppManager nativeApplicationManager = new NativeAppManager(executionId);
        nativeApplicationManager.registerNativeApps(nativeApplications);

        return nativeApplicationManager;
    }

    /**
     * Builds the WAVE simulation parameter map.
     * 
     * @return A map of WAVE simulation parameter names and values.
     */
    static Map<String, String> buildWaveParameters() {

        Map<String, String> parameterMap = new HashMap<String, String>();

        try {

            InitialContext context = new InitialContext();
            parameterMap.put(Constants.NS3_BUILD_DIR, (String)context.lookup(Constants.NS3_BUILD_DIR));
            parameterMap.put(Constants.NS3_CYGWIN_HOME, (String)context.lookup(Constants.NS3_CYGWIN_HOME));
        }
        catch (NamingException exception) {

            /*
             * Ignore the exception. A linux user probably neglected to add cygwin_home or ns3
             * hasn't been installed/configured. If there's actually a problem, it should surface
             * when/if ns3 runs.
             */
        }

        return parameterMap;
    }

    /**
     * Builds the map of simulators for the specified execution.
     * 
     * @param factory The simulation factory to use.
     * @param composite The parent composite.
     * @param executionId The long ID of the execution.
     * @return A map of simulators for the specified execution.
     * @throws WebAppException If the map of simulators cannot be built.
     */
    static Map<Long, SimulatorInterface> buildSimulatorMap(SimFactory factory, Composite composite, Long executionId) throws WebAppException {

        Map<Long, SimulatorInterface> simulatorMap = new HashMap<Long, SimulatorInterface>();

        for (Simulation simulation : composite.getSimulations()) {

            try {

                String uuid = String.format("exec%dsim%d", executionId, simulation.getId());
                simulatorMap.put(simulation.getId(), SimulationBuilder.buildSimulator(factory, uuid, simulation, ExecutionBuilder.SEED_GENERATOR.nextInt()));
            }
            catch (SimResourceException exception) {

                String exceptionTitle = "Build Simulator Map Failure";
                LoggerFactory.getLogger(ExecutionBuilder.class).error(exceptionTitle, exception);
                String exceptionMessage = (exception.getErrorType() == SimResourceErrorType.ALLOCATE)
                        ? "A connection could not be allocated for the new execution."
                        : "An interface to the new execution could not be created.";

                throw new WebAppException(exceptionTitle, exceptionMessage);
            }
        }

        return simulatorMap;
    }
}
