package com.harmonia.qa.ETEXASWebQATests;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.AppParameter;
import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.ETEXASWebQATests.entities.ReportDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.Signal;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationFoundation;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationType;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation.Template;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneID;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneMovement;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneType;
import com.harmonia.qa.ETEXASWebQATests.enums.SignalType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasFileUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasTestBase;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Base data creation for ETexas tests
 *
 * @author llaroussini
 */
public class ETexasBaseData extends ETexasAfterTestResetTestBase {

    /**
     * Output file where entities will be serialized and saved as JSON
     */
    private static File outputFile = new File(ETEXAS_JSON_DATA_FILE);

    /**
     * Executes first to clear previous file contents
     *
     * @throws FileNotFoundException If the given file object does not denote an
     *         existing, writable regular file and a new regular file of that
     *         name cannot be created, or if some other error occurs while
     *         opening or creating the file
     */
    @Before
    public void warmUp() throws FileNotFoundException {
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        ETexasFileUtils.clearFileContents(outputFile);
        ETexasTestBase.setJsonFileName(ETEXAS_JSON_DATA_FILE);
    }

    /**
     * Creates serializable entity instances of pre-loaded base data
     */
    @Test
    public void createBaseData() {
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Create Base Data");
        //Ball signals
        Signal ballSignal = new Signal();
        ballSignal.setSignalType(SignalType.BALL);
        ETexasEntityManager.addEntity(ballSignal);

        //Detector
        Detector detector = new Detector();
        detector.setWidth("200");
        detector.setHeight("100");
        detector.setDistance("100");
        ETexasEntityManager.addEntity(detector);

        //Default Lanes
        List<Lane> lanes = new ArrayList<Lane>(16);
        Lane lane1 = new Lane();
        lane1.setLaneID(LaneID.ONE);
        lane1.setLaneApproach("1");
        lane1.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements1 = new ArrayList<LaneMovement>(2);
        movements1.add(LaneMovement.LEFT_TURN);
        movements1.add(LaneMovement.STRAIGHT);
        lane1.setLaneMovements(movements1);
        lane1.setSpeedLimit("13.411200000000001");
        lane1.setLaneStartNode("(-182.88, 1341.52, 0, 365)");
        lane1.setLaneEndNode("(-182.88, 25725.12, 0, 365)");
        lane1.setSignal(ballSignal);
        lane1.setDetector(detector);
        detector.setLane(lane1);
        lanes.add(lane1);
        ETexasEntityManager.addEntity(lane1);

        Lane lane2 = new Lane();
        lane2.setLaneID(LaneID.TWO);
        lane2.setLaneApproach("1");
        lane2.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements2 = new ArrayList<LaneMovement>(2);
        movements2.add(LaneMovement.RIGHT_TURN);
        movements2.add(LaneMovement.STRAIGHT);
        lane2.setLaneMovements(movements2);
        lane2.setSpeedLimit("13.411200000000001");
        lane2.setLaneStartNode("(-548.64, 1341.52, 0, 365)");
        lane2.setLaneEndNode("(-548.64, 25725.12, 0, 365)");
        lane2.setSignal(ballSignal);
        lanes.add(lane2);
        ETexasEntityManager.addEntity(lane2);

        Lane lane3 = new Lane();
        lane3.setLaneID(LaneID.THREE);
        lane3.setLaneApproach("8");
        lane3.setLaneType(LaneType.OUTBOUND);
        lane3.setSpeedLimit("13.411200000000001");
        lane3.setLaneStartNode("(-8961.12, 182.88, 0, 365)");
        lane3.setLaneEndNode("(-1341.1200000000001, 182.88, 0, 365)");
        lanes.add(lane3);
        ETexasEntityManager.addEntity(lane3);

        Lane lane4 = new Lane();
        lane4.setLaneID(LaneID.FOUR);
        lane4.setLaneApproach("8");
        lane4.setLaneType(LaneType.OUTBOUND);
        lane4.setSpeedLimit("13.411200000000001");
        lane4.setLaneStartNode("(-8961.12, 548.64, 0, 365)");
        lane4.setLaneEndNode("(-1341.1200000000001, 548.64, 0, 365)");
        lanes.add(lane4);
        ETexasEntityManager.addEntity(lane4);

        Lane lane5 = new Lane();
        lane5.setLaneID(LaneID.FIVE);
        lane5.setLaneApproach("2");
        lane5.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements5 = new ArrayList<LaneMovement>(2);
        movements5.add(LaneMovement.LEFT_TURN);
        movements5.add(LaneMovement.STRAIGHT);
        lane5.setLaneMovements(movements5);
        lane5.setSpeedLimit("13.411200000000001");
        lane5.setLaneStartNode("(1341.52, 182.88, 0, 365)");
        lane5.setLaneEndNode("(25725.12, 182.88, 0, 365)");
        lane5.setSignal(ballSignal);
        lanes.add(lane5);
        ETexasEntityManager.addEntity(lane5);

        Lane lane6 = new Lane();
        lane6.setLaneID(LaneID.SIX);
        lane6.setLaneApproach("2");
        lane6.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements6 = new ArrayList<LaneMovement>(2);
        movements6.add(LaneMovement.RIGHT_TURN);
        movements6.add(LaneMovement.STRAIGHT);
        lane6.setLaneMovements(movements6);
        lane6.setSpeedLimit("13.411200000000001");
        lane6.setLaneStartNode("(1341.52, 548.64, 0, 365)");
        lane6.setLaneEndNode("(25725.12, 548.64, 0, 365)");
        lane6.setSignal(ballSignal);
        lanes.add(lane6);
        ETexasEntityManager.addEntity(lane6);

        Lane lane7 = new Lane();
        lane7.setLaneID(LaneID.SEVEN);
        lane7.setLaneApproach("5");
        lane7.setLaneType(LaneType.OUTBOUND);
        lane7.setSpeedLimit("13.411200000000001");
        lane7.setLaneStartNode("(182.88, 8961.12, 0, 365)");
        lane7.setLaneEndNode("(182.88, 1341.1200000000001, 0, 365)");
        lanes.add(lane7);
        ETexasEntityManager.addEntity(lane7);

        Lane lane8 = new Lane();
        lane8.setLaneID(LaneID.EIGHT);
        lane8.setLaneApproach("5");
        lane8.setLaneType(LaneType.OUTBOUND);
        lane8.setSpeedLimit("13.411200000000001");
        lane8.setLaneStartNode("(548.64, 8961.12, 0, 365)");
        lane8.setLaneEndNode("(548.64, 1341.1200000000001, 0, 365)");
        lanes.add(lane8);
        ETexasEntityManager.addEntity(lane8);

        Lane lane9 = new Lane();
        lane9.setLaneID(LaneID.NINE);
        lane9.setLaneApproach("3");
        lane9.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements9 = new ArrayList<LaneMovement>(2);
        movements9.add(LaneMovement.LEFT_TURN);
        movements9.add(LaneMovement.STRAIGHT);
        lane9.setLaneMovements(movements9);
        lane9.setSpeedLimit("13.411200000000001");
        lane9.setLaneStartNode("(182.88, -1341.52, 0, 365)");
        lane9.setLaneEndNode("(182.88, -25725.12, 0, 365)");
        lane9.setSignal(ballSignal);
        lanes.add(lane9);
        ETexasEntityManager.addEntity(lane9);

        Lane lane10 = new Lane();
        lane10.setLaneID(LaneID.TEN);
        lane10.setLaneApproach("3");
        lane10.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements10 = new ArrayList<LaneMovement>(2);
        movements10.add(LaneMovement.RIGHT_TURN);
        movements10.add(LaneMovement.STRAIGHT);
        lane10.setLaneMovements(movements10);
        lane10.setSpeedLimit("13.411200000000001");
        lane10.setLaneStartNode("(548.64, -1341.52, 0, 365)");
        lane10.setLaneEndNode("(548.64, -25725.12, 0, 365)");
        lane10.setSignal(ballSignal);
        lanes.add(lane10);
        ETexasEntityManager.addEntity(lane10);

        Lane lane11 = new Lane();
        lane11.setLaneID(LaneID.ELEVEN);
        lane11.setLaneApproach("6");
        lane11.setLaneType(LaneType.OUTBOUND);
        lane11.setSpeedLimit("13.411200000000001");
        lane11.setLaneStartNode("(8961.12, -182.88, 0, 365)");
        lane11.setLaneEndNode("(1341.1200000000001, -182.88, 0, 365)");
        lanes.add(lane11);
        ETexasEntityManager.addEntity(lane11);

        Lane lane12 = new Lane();
        lane12.setLaneID(LaneID.TWELVE);
        lane12.setLaneApproach("6");
        lane12.setLaneType(LaneType.OUTBOUND);
        lane12.setSpeedLimit("13.411200000000001");
        lane12.setLaneStartNode("(8961.12, -548.64, 0, 365)");
        lane12.setLaneEndNode("(1341.1200000000001, -548.64, 0, 365)");
        lanes.add(lane12);
        ETexasEntityManager.addEntity(lane12);

        Lane lane13 = new Lane();
        lane13.setLaneID(LaneID.THIRTEEN);
        lane13.setLaneApproach("4");
        lane13.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements13 = new ArrayList<LaneMovement>(2);
        movements13.add(LaneMovement.LEFT_TURN);
        movements13.add(LaneMovement.STRAIGHT);
        lane13.setLaneMovements(movements13);
        lane13.setSpeedLimit("13.411200000000001");
        lane13.setLaneStartNode("(-1341.52, -182.88, 0, 365)");
        lane13.setLaneEndNode("(-25725.12, -182.88, 0, 365)");
        lane13.setSignal(ballSignal);
        lanes.add(lane13);
        ETexasEntityManager.addEntity(lane13);

        Lane lane14 = new Lane();
        lane14.setLaneID(LaneID.FOURTEEN);
        lane14.setLaneApproach("4");
        lane14.setLaneType(LaneType.INBOUND);
        List<LaneMovement> movements14 = new ArrayList<LaneMovement>(2);
        movements14.add(LaneMovement.RIGHT_TURN);
        movements14.add(LaneMovement.STRAIGHT);
        lane14.setLaneMovements(movements14);
        lane14.setSpeedLimit("13.411200000000001");
        lane14.setLaneStartNode("(-1341.52, -548.64, 0, 365)");
        lane14.setLaneEndNode("(-25725.12, -548.64, 0, 365)");
        lane14.setSignal(ballSignal);
        lanes.add(lane14);
        ETexasEntityManager.addEntity(lane14);

        Lane lane15 = new Lane();
        lane15.setLaneID(LaneID.FIFTEEN);
        lane15.setLaneApproach("7");
        lane15.setLaneType(LaneType.OUTBOUND);
        lane15.setSpeedLimit("13.411200000000001");
        lane15.setLaneStartNode("(-182.88, -8961.12, 0, 365)");
        lane15.setLaneEndNode("(-182.88, -1341.1200000000001, 0, 365)");
        lanes.add(lane15);
        ETexasEntityManager.addEntity(lane15);

        Lane lane16 = new Lane();
        lane16.setLaneID(LaneID.SIXTEEN);
        lane16.setLaneApproach("7");
        lane16.setLaneType(LaneType.OUTBOUND);
        lane16.setSpeedLimit("13.411200000000001");
        lane16.setLaneStartNode("(-548.64, -8961.12, 0, 365)");
        lane16.setLaneEndNode("(-548.64, -1341.1200000000001, 0, 365)");
        lanes.add(lane16);
        ETexasEntityManager.addEntity(lane16);

        //Default simulation (still needs to be created - but the data points associated are default values)
        List<TemplateSimulation> templateSims = new ArrayList<TemplateSimulation>();
        TemplateSimulation sim = new TemplateSimulation();
        sim.setName(defaultEx05Sim);
        sim.setSimFoundation(SimulationFoundation.TEMPLATE);
        sim.setSimType(SimulationType.ETEXAS);
        sim.setTemplate(Template.EX_05);
        sim.setLanes(lanes);
        templateSims.add(sim);
        CompositeSimulation composite = new CompositeSimulation();
        composite.setName(RandomStringGenerator.nextLetterString(10));
        composite.setTemplateSims(templateSims);
        sim.setComposite(composite);
        ETexasEntityManager.addEntities(sim, composite);

        //Default Report Device configured on all simulations by default
        ReportDevice reportDevice = new ReportDevice();
        reportDevice.setName(defaultReportDevice);
        ETexasEntityManager.addEntity(reportDevice);

        //Setup embedded apps
        //BSMProducerApp
        EmbeddedApp bsmProducerApp = new EmbeddedApp();
        bsmProducerApp.setName("BSMProducerApp");
        bsmProducerApp.setDeviceType(DeviceType.OBU);
        List<AppParameter> bsmProducerParams = new ArrayList<AppParameter>(2);
        AppParameter bsmProducerFreq = new AppParameter();
        bsmProducerFreq.setParameterName("frequency");
        bsmProducerFreq.setParameterValue("0.1");
        bsmProducerFreq.setApp(bsmProducerApp);
        bsmProducerParams.add(bsmProducerFreq);
        AppParameter bsmProducerFormattedData = new AppParameter();
        bsmProducerFormattedData.setParameterName("hasFormattedData");
        bsmProducerFormattedData.setParameterValue("false");
        bsmProducerFormattedData.setApp(bsmProducerApp);
        bsmProducerParams.add(bsmProducerFormattedData);
        bsmProducerApp.setParameters(bsmProducerParams);
        ETexasEntityManager.addEntities(bsmProducerFreq, bsmProducerFormattedData, bsmProducerApp);

        //BSMVerboseProducerApp
        EmbeddedApp bsmVerboseApp = new EmbeddedApp();
        bsmVerboseApp.setName("BSMVerboseProducerApp");
        bsmVerboseApp.setDeviceType(DeviceType.OBU);
        List<AppParameter> bsmVerboseParams = new ArrayList<AppParameter>(1);
        AppParameter bsmVerboseFreq = new AppParameter();
        bsmVerboseFreq.setParameterName("frequency");
        bsmVerboseFreq.setParameterValue("0.1");
        bsmVerboseFreq.setApp(bsmVerboseApp);
        bsmVerboseParams.add(bsmVerboseFreq);
        bsmVerboseApp.setParameters(bsmVerboseParams);
        ETexasEntityManager.addEntities(bsmVerboseFreq, bsmVerboseApp);

        //CSRProducerApp
        EmbeddedApp csrApp = new EmbeddedApp();
        csrApp.setName("CSRProducerApp");
        csrApp.setDeviceType(DeviceType.RSE);
        ETexasEntityManager.addEntity(csrApp);

        //HybridIntellifusionMOEApp
        EmbeddedApp hybridApp = new EmbeddedApp();
        hybridApp.setName("HybridIntellifusionMOEApp");
        hybridApp.setDeviceType(DeviceType.RSE);
        ETexasEntityManager.addEntity(hybridApp);

        //MapDataProducerApp
        EmbeddedApp mapDataApp = new EmbeddedApp();
        mapDataApp.setName("MapDataProducerApp");
        mapDataApp.setDeviceType(DeviceType.RSE);
        List<AppParameter> mapParams = new ArrayList<AppParameter>(2);
        AppParameter freqParam = new AppParameter();
        freqParam.setParameterName("frequency");
        freqParam.setParameterValue("1.0");
        freqParam.setApp(mapDataApp);
        AppParameter hasFormattedData = new AppParameter();
        hasFormattedData.setParameterName("hasFormattedData");
        hasFormattedData.setParameterValue("false");
        hasFormattedData.setApp(mapDataApp);
        mapParams.add(freqParam);
        mapParams.add(hasFormattedData);
        mapDataApp.setParameters(mapParams);
        ETexasEntityManager.addEntities(freqParam, hasFormattedData, mapDataApp);

        //MicroscopicModelApp
        EmbeddedApp microModelApp = new EmbeddedApp();
        microModelApp.setName("MicroscopicModelApp");
        microModelApp.setDeviceType(DeviceType.RSE);
        ETexasEntityManager.addEntity(microModelApp);

        //MsgRxAppByDestination
        EmbeddedApp msgRxAppByDestinationApp = new EmbeddedApp();
        msgRxAppByDestinationApp.setName("MsgRxAppByDestination");
        msgRxAppByDestinationApp.setDeviceType(DeviceType.REPORT);
        ETexasEntityManager.addEntity(msgRxAppByDestinationApp);

        //MsgRxAppBySource
        EmbeddedApp msgRxAppBySourceApp = new EmbeddedApp();
        msgRxAppBySourceApp.setName("MsgRxAppBySource");
        msgRxAppBySourceApp.setDeviceType(DeviceType.REPORT);
        ETexasEntityManager.addEntity(msgRxAppBySourceApp);

        //MsgTxAppByDestination
        EmbeddedApp msgTxAppByDestinationApp = new EmbeddedApp();
        msgTxAppByDestinationApp.setName("MsgTxAppByDestination");
        msgTxAppByDestinationApp.setDeviceType(DeviceType.REPORT);
        ETexasEntityManager.addEntity(msgTxAppByDestinationApp);

        //MsgTxAppBySource
        EmbeddedApp msgTxAppBySourceApp = new EmbeddedApp();
        msgTxAppBySourceApp.setName("MsgTxAppBySource");
        msgTxAppBySourceApp.setDeviceType(DeviceType.REPORT);
        ETexasEntityManager.addEntity(msgTxAppBySourceApp);

        //ReportMOEApp
        EmbeddedApp reportMOEApp = new EmbeddedApp();
        reportMOEApp.setName("ReportMOEApp");
        reportMOEApp.setDeviceType(DeviceType.REPORT);
        List<AppParameter> reportParams = new ArrayList<AppParameter>(1);
        AppParameter lengthParam = new AppParameter();
        lengthParam.setParameterName("lengthFromIntersection");
        lengthParam.setParameterValue("20000");
        lengthParam.setApp(reportMOEApp);
        reportParams.add(lengthParam);
        reportMOEApp.setParameters(reportParams);
        ETexasEntityManager.addEntities(lengthParam, reportMOEApp);

        //SPATProducerApp
        EmbeddedApp spatProducerApp = new EmbeddedApp();
        spatProducerApp.setName("SPATProducerApp");
        spatProducerApp.setDeviceType(DeviceType.RSE);
        List<AppParameter> spatProducerParams = new ArrayList<AppParameter>(2);
        AppParameter spatFreq = new AppParameter();
        spatFreq.setParameterName("frequency");
        spatFreq.setParameterValue("1.0");
        spatFreq.setApp(spatProducerApp);
        spatProducerParams.add(spatFreq);
        AppParameter hasFormattedDataSPAT = new AppParameter();
        hasFormattedDataSPAT.setParameterName("hasFormattedData");
        hasFormattedDataSPAT.setParameterValue("false");
        hasFormattedDataSPAT.setApp(spatProducerApp);
        spatProducerParams.add(hasFormattedDataSPAT);
        spatProducerApp.setParameters(spatProducerParams);
        ETexasEntityManager.addEntities(spatFreq, hasFormattedDataSPAT, spatProducerApp);

        //VehicleInfoByIdApp
        EmbeddedApp vehicleInfoByIdApp = new EmbeddedApp();
        vehicleInfoByIdApp.setName("VehicleInfoByIdApp");
        vehicleInfoByIdApp.setDeviceType(DeviceType.REPORT);
        ETexasEntityManager.addEntity(vehicleInfoByIdApp);

        //VehicleInfoByLaneApp
        EmbeddedApp vehicleInfoByLaneApp = new EmbeddedApp();
        vehicleInfoByLaneApp.setName("VehicleInfoByLaneApp");
        vehicleInfoByLaneApp.setDeviceType(DeviceType.REPORT);
        ETexasEntityManager.addEntity(vehicleInfoByLaneApp);

        //Default Admin User
        ETexasUser admin = new ETexasUser();
        admin.setUsername(defaultAdminUsername);
        admin.setPassword(defaultAdminPassword);
        ETexasEntityManager.addEntity(admin);
    }

    /**
     * Cooldown method for writing out data after the test has run
     *
     * @throws IOException if errors are encountered writing to the file
     */
    @After
    public void coolDown() throws IOException {
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Cool Down");
        File outputFile = new File(ETEXAS_JSON_DATA_FILE);
        ETexasFileUtils.writeToFile(ETexasEntityManager.getEntities(), outputFile);
    }

}
