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
package com.harmonia.apps;

import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.PropertyException;

import org.apache.commons.codec.CharEncoding;
import org.etexascode.apps.MsgByteCount;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsStringOnModel;
import org.etexascode.j2735.AccelSteerYawRateConfidence;
import org.etexascode.j2735.AccelerationConfidence;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.BrakeAppliedPressure;
import org.etexascode.j2735.BumperHeights;
import org.etexascode.j2735.CommonSafetyRequest;
import org.etexascode.j2735.ConfidenceSet;
import org.etexascode.j2735.DDateTime;
import org.etexascode.j2735.EssPrecipSituation;
import org.etexascode.j2735.EssPrecipYesNo;
import org.etexascode.j2735.FullPositionVector;
import org.etexascode.j2735.J1939Data;
import org.etexascode.j2735.J1939Data.Axle;
import org.etexascode.j2735.J1939Data.Tires;
import org.etexascode.j2735.LightbarInUse;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.PathHistory;
import org.etexascode.j2735.PathPrediction;
import org.etexascode.j2735.RTCMPackage;
import org.etexascode.j2735.RainSensor;
import org.etexascode.j2735.SPAT;
import org.etexascode.j2735.SpeedConfidence;
import org.etexascode.j2735.SteeringWheelAngleConfidence;
import org.etexascode.j2735.ThrottleConfidence;
import org.etexascode.j2735.TimeConfidence;
import org.etexascode.j2735.VehicleIdent;
import org.etexascode.j2735.VehicleSafetyExtension;
import org.etexascode.j2735.VehicleStatus;
import org.etexascode.j2735.WheelSensorStatus;
import org.etexascode.j2735.YawRateConfidence;
import org.etexascode.nonstd.Message;

/**
 * Utilities for both the J2735 message producer apps.
 * 
 * @author jrutherford
 */
public class UtilsProducer {

    private UtilsProducer() {};

    /**
     * Adds the extended data to the BSM.
     * 
     * @param simtime The simulation time.
     * @param bsm The message to add the data to.
     * @param vehicle The vehicle information.
     * @return The size of the extended data
     * @throws UnsupportedEncodingException If the extended data cannot be added.
     */
    public static double addExtendedData(Double simtime, Message bsm, IVehicle vehicle) throws UnsupportedEncodingException {

        VehicleSafetyExtension vsm = new VehicleSafetyExtension();

        double tempSize = 0.0;
        // Sets the path prediction values.
        // These values are both set to 0 because
        // they are impossible to calculate without
        // vehicle history and unavailable.
        PathPrediction patPred = new PathPrediction();
        patPred.setRadiusOfCurve((short)0);
        patPred.setConfidence((short)0);

        vsm.setPathPrediction(patPred);

        // increment msg byte size
        tempSize += increment(MsgByteCount.PATHPREDICTION);

        vsm.setEvents(0);
        // increment msg byte size
        tempSize += increment(MsgByteCount.EVENTFLAGS);

        VehicleStatus status = new VehicleStatus();

        // Wipers object.
        VehicleStatus.Wipers wipers = new VehicleStatus.Wipers();
        wipers.setStatusFront("0");
        wipers.setStatusRear("0");
        wipers.setRateFront((short)0);
        wipers.setRateRear((short)0);
        status.setWipers(wipers);

        // increment msg byte size
        tempSize += increment(MsgByteCount.WIPERSTATUSFRONT) + increment(MsgByteCount.WIPERSTATUSREAR) + increment(MsgByteCount.WIPERRATE);

        // Steering object.
        VehicleStatus.Steering steering = new VehicleStatus.Steering();
        steering.setConfidence(SteeringWheelAngleConfidence.UNAVAILABLE);
        steering.setRate((byte)0x0);
        steering.setAngle(new byte[] { (byte)0x7F });
        steering.setWheels((byte)0x0);
        status.setSteering(steering);

        // increment msg byte size
        tempSize += increment(MsgByteCount.STEERINGWHEELANGLE);
        tempSize += increment(MsgByteCount.STEERINGWHEELRATEOFCHANGE);

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

        // increment msg byte size
        tempSize += increment(MsgByteCount.ACCELERATIONCONFIDENCE);
        tempSize += increment(MsgByteCount.YAWRATECONFIDENCE);
        tempSize += increment(MsgByteCount.STEERINGWHEELANGLECONFIDENCE);
        tempSize += increment(MsgByteCount.CONFIDENCESET);

        // Vehicle Data Object.
        VehicleStatus.VehicleData vd = new VehicleStatus.VehicleData();
        BumperHeights bheights = new BumperHeights();
        bheights.setFrnt((short)(vehicle.getHeight() / 0.01));
        bheights.setRear((short)(vehicle.getHeight() / 0.01));
        vd.setBumpers(bheights);
        vd.setHeight((short)0);
        vd.setMass((short)0);
        vd.setTrailerWeight(0);
        vd.setType(UtilsStringOnModel.decodeUTF8(new byte[] { (byte)vehicle.getType().getValue() }));
        status.setVehicleData(vd);

        // increment msg byte size
        tempSize += increment(MsgByteCount.BUMPERHEIGHTS);
        tempSize += increment(MsgByteCount.VEHCILEHEIGHT);
        tempSize += increment(MsgByteCount.VEHICLEMASS);
        tempSize += increment(MsgByteCount.TRAILERWEIGHT);
        tempSize += increment(MsgByteCount.VEHICLETYPE);

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

        // increment msg byte size.Vechicle class is an external data type
        tempSize += increment(MsgByteCount.ITISEXTERNAL);
        tempSize += increment(MsgByteCount.TEMPORARYID);

        // TODO increment variable length strings
        // increment variable length strings, 7 bit chars
        // tempSize +=7*name.length()+ownercode.length();
        // increment variable length strings, 8 bit chars
        tempSize += 1;
        // increment msg byte size
        tempSize += increment(MsgByteCount.VEHICLETYPE);

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

        // increment msg byte size. J1939 data is a constant 27 byte size.
        tempSize += increment(MsgByteCount.J1939DATA);

        // Weather report object.
        VehicleStatus.WeatherReport weather = new VehicleStatus.WeatherReport();
        weather.setFriction((short)0);
        weather.setIsRaining(EssPrecipYesNo.ERROR);
        weather.setPrecipSituation(EssPrecipSituation.UNKNOWN);
        weather.setRainRate(0);
        weather.setSolarRadiation(0);
        status.setWeatherReport(weather);

        // increment msg byte size. weather data is a constant 13 byte size.
        tempSize += increment(MsgByteCount.WEATHER);

        // GPS Status.
        status.getGpsStatus().clear();
        status.getGpsStatus().add("0");

        // increment msg byte size.
        tempSize += increment(MsgByteCount.GPSSTATUS);

        // Other status variables.
        status.setLights(0);
        status.setLightBar(LightbarInUse.UNAVAILABLE);
        status.setThrottlePos((short)0);
        status.setSpeedC(SpeedConfidence.UNAVAILABLE);
        status.setBrakePressure(BrakeAppliedPressure.UNAVAILABLE);
        status.setRoadFriction((short)0);
        status.setSunData(0);
        status.setRainData(RainSensor.NONE);
        status.setAirPres((short)0);
        status.setAirTemp((short)0);

        // increment msg byte size.
        tempSize += increment(MsgByteCount.EXTERIORLIGHTS);
        tempSize += increment(MsgByteCount.LIGHTBARINUSE);
        tempSize += increment(MsgByteCount.THROTTLEPOSITION);
        tempSize += increment(MsgByteCount.SPEEDCONFIDENCE);
        tempSize += increment(MsgByteCount.BRAKEAPPLIEDPRESSURE);
        tempSize += increment(MsgByteCount.COEFFICIENTOFFRICTION);
        tempSize += increment(MsgByteCount.SUNSENSOR);
        tempSize += increment(MsgByteCount.RAINSENSOR);
        tempSize += increment(MsgByteCount.AMBIENTAIRPRESSURE);
        tempSize += increment(MsgByteCount.AMBIENTAIRTEMPERATURE);

        FullPositionVector fpv = new FullPositionVector();

        fpv.setPosAccuracy(new byte[] { (byte)0xff, (byte)0xff, (byte)0xff, (byte)0xff });
        fpv.setTimeConfidence(TimeConfidence.UNAVAILABLE);
        fpv.setPosConfidence((new byte[] { (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x00 }));
        fpv.setSpeedConfidence((new byte[] { (byte)0x00, (byte)0x00, (byte)0x00 }));

        // increment msg byte size.
        tempSize += increment(MsgByteCount.POSITIONALACCURACY);
        tempSize += increment(MsgByteCount.TIMECONFIDENCE);
        tempSize += increment(MsgByteCount.POSITIONCONFIDENCESET);
        tempSize += increment(MsgByteCount.SPEEDCONFIDENCE);

        PathHistory patHis = new PathHistory();

        // Not sure how to set crumb data unavailable.
        // Spec calls for 18 bits lat and 18 bits long.
        // With -131072 as the unavailable number.
        int unavail = -131072 << 18;
        PathHistory.CrumbData cdata = new PathHistory.CrumbData();
        cdata.setPathHistoryPointSets10(new byte[] { (byte)(unavail >> 10), (byte)(unavail >> 2), (byte)((unavail) + (unavail >> 12)), (byte)(unavail >> 4), (byte)((short)unavail) });
        patHis.setCrumbData(cdata);
        patHis.setItemCnt((short)1);
        patHis.getCurrGPSstatus().clear();
        patHis.getCurrGPSstatus().add("0"); // Unavailable

        // increment msg byte size.
        tempSize += increment(MsgByteCount.PATHHISTORY);

        RTCMPackage rtcm = new RTCMPackage();

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

        // increment msg byte size.
        tempSize += increment(MsgByteCount.RTCMREVISION);
        tempSize += increment(MsgByteCount.RTCMID);
        // TODO add non-empty payload

        VehicleStatus.Object obj = new VehicleStatus.Object();
        obj.setObDirect(0);
        obj.setObDist(0);

        // increment msg byte size.
        tempSize += increment(MsgByteCount.OBSTACLEDIRECTION);
        tempSize += increment(MsgByteCount.OBSTACLEDISTANCE);

        // Sets the DDateTime object values.
        Date now = new Date(simtime.longValue());
        DDateTime date = new DDateTime();
        date.setYear(now.getYear());
        date.setMonth((short)now.getMonth());
        date.setDay((short)now.getDay());
        date.setHour((short)now.getHours());
        date.setMinute((short)now.getMinutes());
        date.setSecond(now.getSeconds());

        // increment msg byte size.
        tempSize += increment(MsgByteCount.DDATETIME);

        // Sets the FullPositionVector values.
        fpv.setUtcTime(date);
        fpv.setLat((int)vehicle.getLatitude());
        fpv.setLong((int)vehicle.getLongitude());
        fpv.setElevation(ByteBuffer.wrap(new byte[8]).putDouble(vehicle.getElev()).array());
        fpv.setHeading((int)vehicle.getHeading());
        fpv.setSpeed(ByteBuffer.wrap(new byte[8]).putDouble(vehicle.getSpeed()).array());

        // Sets the path history values.
        patHis.setInitialPosition(fpv);

        // increment msg byte size.
        tempSize += increment(MsgByteCount.FULLPOSITIONVECTOR);

        // Sets the RTCM values.
        // These are all set to null because this object
        // is needed. It relates to satellite data.
        rtcm.setAnchorPoint(fpv);

        // Sets the VSM values.
        vsm.setPathHistory(patHis);
        vsm.setTheRTCM(rtcm);

        // Vehicle Object object.
        obj.setDateTime(date);
        status.setObject(obj);

        // Other status variables.
        status.setFullPos(fpv);
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
        // increment msg byte size.
        tempSize += increment(MsgByteCount.STATUS);
        return tempSize;
    }

    /**
     * Increments using the MsgByteCount enum
     * 
     * @param msgByteCount field from enum MsgByteCount
     * @return double byte size of field
     */
    public static double increment(MsgByteCount msgByteCount) {
        return msgByteCount.getByteCount();
    }

    /**
     * Get json String from message
     * 
     * @param bsm Basic Safety Message
     * @return json string for display
     * @throws JAXBException If the basic safety message cannot be turned into a JSON string.
     */
    public static String jsonBSM(BasicSafetyMessage bsm) throws JAXBException {
        String jsonMessage = null;
        StringWriter writer = new StringWriter();
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("eclipselink.media-type", "application/json");
        properties.put("eclipselink.json.include-root", false);
        JAXBContext jc;
        jc = JAXBContext.newInstance(new Class[] { BasicSafetyMessage.class }, properties);

        Marshaller marshaller = jc.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        marshaller.marshal(bsm, writer);
        jsonMessage = writer.toString();
        return jsonMessage;
    }

    /**
     * Get json String from message
     * 
     * @param bsmv Basic Safety Message Verbose
     * @return json string for display
     * @throws JAXBException If the basic safety message verbose cannot be turned into a JSON
     *         string.
     * @throws PropertyException If the basic safety message verbose cannot be turned into a JSON
     *         string.
     */
    public static String jsonBSMV(BasicSafetyMessageVerbose bsmv) throws JAXBException {
        String jsonMessage = null;
        StringWriter writer = new StringWriter();
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("eclipselink.media-type", "application/json");
        properties.put("eclipselink.json.include-root", false);
        JAXBContext jc;
        jc = JAXBContext.newInstance(new Class[] { BasicSafetyMessageVerbose.class }, properties);

        Marshaller marshaller = jc.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        marshaller.marshal(bsmv, writer);
        jsonMessage = writer.toString();
        return jsonMessage;
    }

    /**
     * Get json String from message
     * 
     * @param spat SPAT Message
     * @return json string for display
     * @throws JAXBException If the Signal phase and timing message cannot be turned into a JSON
     *         string.
     */
    public static String jsonSPAT(SPAT spat) throws JAXBException {
        String jsonMessage = null;
        JAXBContext jc;

        StringWriter writer = new StringWriter();
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("eclipselink.media-type", "application/json");
        jc = JAXBContext.newInstance(new Class[] { SPAT.class }, properties);
        Marshaller marshaller = jc.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        marshaller.marshal(spat, writer);
        jsonMessage = writer.toString();

        return jsonMessage;
    }

    /**
     * Get json string from message
     * 
     * @param mapdata MapData message
     * @return json string for display
     * @throws JAXBException If the map data message cannot be turned into a JSON string.
     */
    public static String jsonMapData(MapData mapdata) throws JAXBException {
        String jsonMessage = null;
        JAXBContext jc;

        StringWriter writer = new StringWriter();
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("eclipselink.media-type", "application/json");
        jc = JAXBContext.newInstance(new Class[] { MapData.class }, properties);
        Marshaller marshaller = jc.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        marshaller.marshal(mapdata, writer);
        jsonMessage = writer.toString();

        return jsonMessage;
    }

    /**
     * Get json string from message
     * 
     * @param csr Common Safety Request message
     * @return json string for display
     * @throws JAXBException If the map data message cannot be turned into a JSON string.
     */
    public static String jsonCSR(CommonSafetyRequest csr) throws JAXBException {
        String jsonMessage = null;
        JAXBContext jc;

        StringWriter writer = new StringWriter();
        Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("eclipselink.media-type", "application/json");
        jc = JAXBContext.newInstance(new Class[] { CommonSafetyRequest.class }, properties);
        Marshaller marshaller = jc.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        marshaller.marshal(csr, writer);
        jsonMessage = writer.toString();

        return jsonMessage;
    }
}