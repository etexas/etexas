/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.test;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.codec.CharEncoding;
import org.etexascode.interrep.datamodel.BSM;
import org.etexascode.interrep.datamodel.BSM.Transmission;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapter;
import org.etexascode.j2735.AccelSteerYawRateConfidence;
import org.etexascode.j2735.AccelerationConfidence;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.BrakeAppliedPressure;
import org.etexascode.j2735.BumperHeights;
import org.etexascode.j2735.ConfidenceSet;
import org.etexascode.j2735.DDateTime;
import org.etexascode.j2735.EssPrecipSituation;
import org.etexascode.j2735.EssPrecipYesNo;
import org.etexascode.j2735.FullPositionVector;
import org.etexascode.j2735.J1939Data;
import org.etexascode.j2735.J1939Data.Axle;
import org.etexascode.j2735.J1939Data.Tires;
import org.etexascode.j2735.LightbarInUse;
import org.etexascode.j2735.PathHistory;
import org.etexascode.j2735.PathPrediction;
import org.etexascode.j2735.RTCMPackage;
import org.etexascode.j2735.RainSensor;
import org.etexascode.j2735.SpeedConfidence;
import org.etexascode.j2735.SteeringWheelAngleConfidence;
import org.etexascode.j2735.ThrottleConfidence;
import org.etexascode.j2735.TimeConfidence;
import org.etexascode.j2735.VehicleIdent;
import org.etexascode.j2735.VehicleSafetyExtension;
import org.etexascode.j2735.VehicleSize;
import org.etexascode.j2735.VehicleStatus;
import org.etexascode.j2735.WheelSensorStatus;
import org.etexascode.j2735.YawRateConfidence;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.nonstd.Message;

//import org.etexascode.interrep.datamodel.Signal;

public class MsgUtil {

    public static SignalManager xmlToSignal(File xmlFile) throws JAXBException {
        JAXBContext context;
        SignalManager signalManager = null;
        context = JAXBContext.newInstance(SignalManager.class);
        final Unmarshaller unmarshaller = context.createUnmarshaller();
        unmarshaller.setAdapter(new ManagerAdapter<SignalIndication>());
        signalManager = (SignalManager)unmarshaller.unmarshal(xmlFile);

        return signalManager;

    }

    // private final Map<Integer, Signal> signals = new HashMap<Integer, Signal>();
    private final List<SignalIndication> signals = new LinkedList<SignalIndication>();

    // create test BSM using BSM bean
    public static BSM testcaseBSM() {
        BSM bsmExample = new BSM();

        bsmExample.setAccuracy(10);
        bsmExample.setBrakeStatus((short)0);
        bsmExample.setDSRCmsgID(DSRCMessageID.MSG_ID_BSM);
        bsmExample.setElevation((short)610);
        bsmExample.setHeading((short)5);
        bsmExample.setLatitude(37.22955);
        bsmExample.setLateralAcceleration((short)0);
        bsmExample.setLongitude(-80.41396);
        bsmExample.setLongitudeAcceleration((short)0);
        bsmExample.setMsgCount(25);
        bsmExample.setSecond((short)3980);
        bsmExample.setSpeed(35.00);
        bsmExample.setSteeringAngle(5);
        bsmExample.setTempID(10);
        bsmExample.setTransmission(Transmission.FORWARDGEARS);
        bsmExample.setVehicleLength(180);
        bsmExample.setVehicleWidth(80);
        bsmExample.setVerticalAcceleration((short)5);
        bsmExample.setYaw(0);
        return bsmExample;

    }

    static BasicSafetyMessageVerbose getBasicSafetyMessageVerbose(IVehicle vehicle, boolean extended, short msgCount) throws UnsupportedEncodingException {
        BasicSafetyMessageVerbose bsm = new BasicSafetyMessageVerbose();
        bsm.setMsgID(DSRCMessageID.MSG_ID_BSM);

        // Create and fill in blob1 as specified in comments below:
        //
        // -- Sent as a single octet blob
        // blob1 BSMblob,
        // --
        // -- The blob consists of the following 38 packed bytes:
        ByteBuffer accelSet7Bytes = ByteBuffer.allocate(7);
        ByteBuffer id4Bytes = ByteBuffer.allocate(4);
        ByteBuffer speed2Bytes = ByteBuffer.allocate(2);
        ByteBuffer brakes2Bytes = ByteBuffer.allocate(2);
        bsm.setMsgCnt(msgCount);
        bsm.setId(id4Bytes.putInt(vehicle.getVehicleID()).array());
        bsm.setSecMark(65535);
        bsm.setLat(UtilsUnitConversion.convertToOneTenthMicrodegree(vehicle.getLatitude())); // convert
                                                                                             // to
                                                                                             // 1/10th
                                                                                             // microdegree
                                                                                             // to
                                                                                             // meet
                                                                                             // spec
        bsm.setLong(UtilsUnitConversion.convertToOneTenthMicrodegree(vehicle.getLongitude()));
        bsm.setElev(new byte[] { (byte)0xF0, 0 }); // encode 0xF000 or "unknown"
        bsm.setAccuracy(new byte[] { 0, 0, 0, 0 });

        int transmissionAndSpeed = 1;
        transmissionAndSpeed <<= 14; // set transmission to 2 Note: TEXAS does not appear to support
                                     // any vehicle transmission state other than forward gears
        // Note: ablatt - converting feet per second to meters per second is the same operaiton as
        // converting feet to meters
        double speed = vehicle.getSpeed();
        speed /= 0.02; // convert speed from meters per second to (0.02)meters per second
        // Note: 8191 == speed unavailable
        if (speed >= 8191) {
            speed = 8191;
        }
        else if (speed < 0) {
            speed = 8191;
        }
        transmissionAndSpeed |= ((int)speed);

        bsm.setSpeed(speed2Bytes.putShort((short)transmissionAndSpeed).array());
        bsm.setHeading((int)Math.round(vehicle.getHeading()));
        // TODO: bbadillo - Need to reinstate the line below when the data is available from
        // InterRep.
        bsm.setAngle(new byte[] { (byte)0 }); // bsm.setAngle(new byte[]
                                              // {(byte)vehicle.getSteeringAngle()});

        accelSet7Bytes.putShort((short)vehicle.getAcceleration());
        accelSet7Bytes.put((byte)0x07); // 0x07D1 = 2001 = "Unavailable" for acceleration
        accelSet7Bytes.put((byte)0xD1);
        accelSet7Bytes.put((byte)-127); // -127 is "Unavailable" for vertical acceleration
        accelSet7Bytes.put((byte)0);
        accelSet7Bytes.put((byte)0);
        bsm.setAccelSet(accelSet7Bytes.array());
        // -- control Control,
        // -- brakes BrakeSystemStatus, -x- 2 bytes
        // TODO: bbadillo - Need to reinstate the line below when the data is available from
        // InterRep.
        // if (vehicle.brakesActive) {
        // BrakeAppliedStatus ::= BIT STRING {
        // allOff (0), -- B'0000 The condition All Off
        // leftFront (1), -- B'0001 Left Front Active
        // leftRear (2), -- B'0010 Left Rear Active
        // rightFront (4), -- B'0100 Right Front Active
        // rightRear (8) -- B'1000 Right Rear Active
        // } -- to fit in 4 bits
        // bsm.setBrakes(brakes2Bytes.putShort((short)15).array()); // This will set the first four
        // bits
        // } else {
        bsm.setBrakes(brakes2Bytes.putShort((short)0).array()); // This indicates no brakes applied
        // }
        // -- basic VehicleBasic,
        // -- size VehicleSize, -x- 3 bytes - to be the vehicle width and the vehicle length (in
        // that order) measured in centimeters
        VehicleSize vehicleSize = new VehicleSize();
        vehicleSize.setWidth((int)vehicle.getWidth());
        vehicleSize.setLength((int)vehicle.getLength());
        bsm.setSize(vehicleSize);

        // Add extended data if CSR recieved.
        if (extended)
            addExtendedData(bsm, vehicle, msgCount);

        return bsm;
    }

    @SuppressWarnings("deprecation")
    public static void addExtendedData(Message bsm, IVehicle vehicle, short msgCount) throws UnsupportedEncodingException {
        VehicleSafetyExtension vsm = new VehicleSafetyExtension();
        VehicleStatus status = new VehicleStatus();

        // Sets the DDateTime object values.
        Date now = new Date();
        DDateTime date = new DDateTime();
        date.setYear(now.getYear());
        date.setMonth((short)now.getMonth());
        date.setDay((short)now.getDay());
        date.setHour((short)now.getHours());
        date.setMinute((short)now.getMinutes());
        date.setSecond(now.getSeconds());

        // Sets the FullPositionVector values.
        FullPositionVector fpv = new FullPositionVector();
        fpv.setUtcTime(date);
        fpv.setLat((int)Math.round(vehicle.getLatitude()));
        fpv.setLong((int)Math.round(vehicle.getLongitude()));
        fpv.setElevation(ByteBuffer.wrap(new byte[8]).putDouble(vehicle.getElev()).array());
        fpv.setHeading((int)Math.round(vehicle.getHeading()));
        fpv.setSpeed(ByteBuffer.wrap(new byte[8]).putDouble(vehicle.getSpeed()).array());
        fpv.setPosAccuracy(new byte[] { (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff });
        fpv.setTimeConfidence(TimeConfidence.UNAVAILABLE);
        fpv.setPosConfidence((new byte[] { (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x00 }));
        fpv.setSpeedConfidence((new byte[] { (byte)0x00, (byte)0x00, (byte)0x00 }));

        // Sets the path history values.
        PathHistory patHis = new PathHistory();
        patHis.setInitialPosition(fpv);

        // Not sure how to set crumb data unavailable.
        // Spec calls for 18 bits lat and 18 bits long.
        // With -131072 as the unavailable number.
        int unavail = -131072 << 18;
        PathHistory.CrumbData cdata = new PathHistory.CrumbData();
        cdata.setPathHistoryPointSets10(new byte[] { (byte)(unavail >> 10), (byte)(unavail >> 2), (byte)((unavail >> 0) + (unavail >> 12)), (byte)(unavail >> 4), (byte)((short)unavail) });
        patHis.setCrumbData(cdata);
        patHis.setItemCnt((short)1);
        patHis.getCurrGPSstatus().clear();
        patHis.getCurrGPSstatus().add("0"); // Unavailable.

        // Sets the path prediction values.
        // These values are both set to 0 because
        // they are impossible to calculate without
        // vehicle history and unavailable.
        PathPrediction patPred = new PathPrediction();
        patPred.setRadiusOfCurve((short)0);
        patPred.setConfidence((short)0);

        // Sets the RTCM values.
        // These are all set to null because this object
        // is needed. It relates to satellite data.
        RTCMPackage rtcm = new RTCMPackage();
        rtcm.setAnchorPoint(fpv);
        // Header 5 bytes.
        // 1 byte GPS Status (0 for unavailable)
        // 4 byte Antenna Offset
        // 14 bits (x), 8191 for unavailable.
        // 9 bits (y), 255 for unavailable.
        // 9 bits (z), 511 for unavailable.
        int aoff = (8191 << 18) | (255 << 9) | 511;
        rtcm.setRtcmHeader(new byte[] { (byte)0x00, (byte)(aoff >> 24), (byte)(aoff >> 16), (byte)(aoff >> 8), (byte)aoff });
        rtcm.setMsg1001(null);
        rtcm.setMsg1002(null);
        rtcm.setMsg1003(null);
        rtcm.setMsg1004(null);
        rtcm.setMsg1005(null);
        rtcm.setMsg1006(null);
        rtcm.setMsg1007(null);
        rtcm.setMsg1008(null);
        rtcm.setMsg1009(null);
        rtcm.setMsg1010(null);
        rtcm.setMsg1011(null);
        rtcm.setMsg1012(null);
        rtcm.setMsg1013(null);
        rtcm.setMsg1014(null);
        rtcm.setMsg1015(null);
        rtcm.setMsg1016(null);
        rtcm.setMsg1017(null);
        rtcm.setMsg1019(null);
        rtcm.setMsg1020(null);
        rtcm.setMsg1021(null);
        rtcm.setMsg1022(null);
        rtcm.setMsg1023(null);
        rtcm.setMsg1024(null);
        rtcm.setMsg1025(null);
        rtcm.setMsg1026(null);
        rtcm.setMsg1027(null);
        rtcm.setMsg1029(null);
        rtcm.setMsg1030(null);
        rtcm.setMsg1031(null);
        rtcm.setMsg1032(null);

        // Sets the VSM values.
        vsm.setEvents(0);
        vsm.setPathHistory(patHis);
        vsm.setPathPrediction(patPred);
        vsm.setTheRTCM(rtcm);

        // Wipers object.
        VehicleStatus.Wipers wipers = new VehicleStatus.Wipers();
        wipers.setStatusFront("0");
        wipers.setStatusRear("0");
        wipers.setRateFront((short)0);
        wipers.setRateRear((short)0);
        status.setWipers(wipers);

        // Steering object.
        VehicleStatus.Steering steering = new VehicleStatus.Steering();
        steering.setConfidence(SteeringWheelAngleConfidence.UNAVAILABLE);
        steering.setRate((byte)0x0);
        steering.setAngle(new byte[] { (byte)0x7F });
        steering.setWheels((byte)0x0);
        status.setSteering(steering);

        // Acceleration object.
        VehicleStatus.AccelSets accel = new VehicleStatus.AccelSets();
        ConfidenceSet cset = new ConfidenceSet();
        AccelSteerYawRateConfidence yawconf = new AccelSteerYawRateConfidence();
        yawconf.setAcceleration(AccelerationConfidence.UNAVAILABLE);
        yawconf.setSteeringWheelAngle(SteeringWheelAngleConfidence.UNAVAILABLE);
        yawconf.setYawRate(YawRateConfidence.UNAVAILABLE);
        cset.setAccelConfidence(yawconf);
        cset.setPosConfidence(new byte[] { (byte)0x0 });
        cset.setSpeedConfidence(new byte[] { (byte)0x0, (byte)0x0, (byte)0x0 });
        cset.setSteerConfidence(SteeringWheelAngleConfidence.UNAVAILABLE);
        cset.setThrottleConfidence(ThrottleConfidence.UNAVAILABLE);
        cset.setTimeConfidence(TimeConfidence.UNAVAILABLE);
        accel.setAccel4Way(new byte[] { (byte)0x0, (byte)0x0, (byte)0x0, (byte)0x0, (byte)0x0, (byte)0x0, (byte)0x0 });
        accel.setConfidenceSet(cset);
        accel.setHozAccelCon(AccelerationConfidence.UNAVAILABLE);
        accel.setYawRateCon(YawRateConfidence.UNAVAILABLE);
        status.setAccelSets(accel);

        // Vehicle Object object.
        VehicleStatus.Object obj = new VehicleStatus.Object();
        obj.setDateTime(date);
        obj.setObDirect(0);
        obj.setObDist(0);
        status.setObject(obj);

        // Vehicle Data Object.
        VehicleStatus.VehicleData vd = new VehicleStatus.VehicleData();
        BumperHeights bheights = new BumperHeights();
        bheights.setFrnt((short)(vehicle.getHeight() / 0.01));
        bheights.setRear((short)(vehicle.getHeight() / 0.01));
        vd.setBumpers(bheights);
        vd.setHeight((short)0);
        vd.setMass((short)0);
        vd.setTrailerWeight(0);
        vd.setType(new String(new byte[] { (byte)vehicle.getType().getValue() }, CharEncoding.UTF_8));
        status.setVehicleData(vd);

        // Vehicle Identity object.
        VehicleIdent ident = new VehicleIdent();
        VehicleIdent.VehicleClass vclass = new VehicleIdent.VehicleClass();
        vclass.setREquip("");
        ident.setId(Integer.toHexString(vehicle.getVehicleID()).getBytes(CharEncoding.UTF_8));
        ident.setName("");
        ident.setOwnerCode("");
        ident.setVehicleClass(vclass);
        ident.setVehicleType(new String(new byte[] { (byte)vehicle.getType().getValue() }, CharEncoding.UTF_8));
        ident.setVin(new byte[] { (byte)0x0 });
        status.setVehicleIdent(ident);

        // J1939 Data object.
        J1939Data jdata = new J1939Data();
        J1939Data.Tires jtires = new J1939Data.Tires();
        Tires.SEQUENCE tseq = new Tires.SEQUENCE();
        tseq.setDetection(new String(new byte[] { (byte)0x0, (byte)0x0, (byte)0x0 }, CharEncoding.UTF_8));
        tseq.setLeakageRate(0);
        tseq.setLocation((short)0);
        tseq.setPressure(0);
        tseq.setTemp(0);
        tseq.setWheelSensorStatus(WheelSensorStatus.NOT_SUPOPRTED);
        jtires.getSEQUENCE().add(tseq);
        Axle axle = new Axle();
        Axle.SEQUENCE seq = new Axle.SEQUENCE();
        seq.setLocation((short)0);
        seq.setWeight(0);
        axle.getSEQUENCE().add(seq);
        jdata.setAxle(axle);
        jdata.setCargoWeight(0);
        jdata.setDriveAxleLiftAirPressure(0);
        jdata.setDriveAxleLocation((short)0);
        jdata.setDriveAxleLubePressure(0);
        jdata.setDriveAxleTemperature((short)0);
        jdata.setSteeringAxleLubePressure((short)0);
        jdata.setSteeringAxleTemperature((short)0);
        jdata.setTires(jtires);
        jdata.setTrailerWeight(0);
        status.setJ1939Data(jdata);

        // Weather report object.
        VehicleStatus.WeatherReport weather = new VehicleStatus.WeatherReport();
        weather.setFriction((short)0);
        weather.setIsRaining(EssPrecipYesNo.ERROR);
        weather.setPrecipSituation(EssPrecipSituation.UNKNOWN);
        weather.setRainRate(0);
        weather.setSolarRadiation(0);
        status.setWeatherReport(weather);

        // GPS Status.
        status.getGpsStatus().clear();
        status.getGpsStatus().add("0");

        // Other status variables.
        status.setLights(0);
        status.setLightBar(LightbarInUse.UNAVAILABLE);
        status.setFullPos(fpv);
        status.setThrottlePos((short)0);
        status.setSpeedC(SpeedConfidence.UNAVAILABLE);
        status.setBrakePressure(BrakeAppliedPressure.UNAVAILABLE);
        status.setRoadFriction((short)0);
        status.setSunData(0);
        status.setRainData(RainSensor.NONE);
        status.setAirPres((short)0);
        status.setAirTemp((short)0);
        // status.setBrakeStatus(null);

        // Adds the BSM extended data.
        if (bsm instanceof BasicSafetyMessage) {
            ((BasicSafetyMessage)bsm).setSafetyExt(vsm);
            ((BasicSafetyMessage)bsm).setStatus(status);
        }
        else if (bsm instanceof BasicSafetyMessageVerbose) {
            ((BasicSafetyMessageVerbose)bsm).setSafetyExt(vsm);
            ((BasicSafetyMessageVerbose)bsm).setStatus(status);
        }
        else {
            throw new IllegalStateException("A message of type " + bsm.getClass().getSimpleName() + " was passed into the BSM Utils Class.");
        }
    }

}
