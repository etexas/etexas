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

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.MsgByteCount;
import org.etexascode.apps.OBUDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.CommonSafetyRequest;
import org.etexascode.j2735.ObjectFactory;
import org.etexascode.j2735.VehicleSize;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.wavesim.WaveMessage;

/**
 * A BSMVerbose producing Connected Vehicle App.
 * 
 * @author bbadillo
 * @author ablatt
 * @author bmauldon
 */
public class BSMVerboseProducerApp implements IOBUBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_BSM_VERBOSE_PRODUCER_APP = "BSMVerboseProducerApp";

    /**
     * Size of message in bytes. This is always 39 bytes for basic safety message verbose, part1.
     */
    private static final int SIZE_PART_1 = (int)Math.ceil(MsgByteCount.DSRCMSGID.getByteCount() + MsgByteCount.BSMBLOB.getByteCount());

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * An incremented counter for the current number of generated messages (modulo 128).
     */
    private short msgCount = 0;

    /**
     * The frequency at which to produce messages in seconds. SPEC states this should never be
     * broadcast in a production environment.
     */
    @AppConfigProperty(value = "0.1", description = "The frequency to send messages in seconds.")
    double frequency;

    /*
     * @Override public boolean matchAppId(String appId, String appConfigs[]) { if
     * (APP_ID_BSMVERBOSE_PRODUCER.equals(appId) && appConfigs.length == 1) { try { frequency =
     * Double.parseDouble(appConfigs[0]); } catch (NumberFormatException ex) {
     * LOGGER.log(Level.WARNING,
     * "Problem matching app[id={}]: {} is not a double precision decimal number" , new
     * Object[]{appId, appConfigs[0]}); return false; } return true; } return false; }
     * @Override public void update(Double simtime) { if (lastTxTime != null) { if ((simtime -
     * lastTxTime) < frequency) { return; } } lastTxTime = simtime; VehicleInfo vehicle =
     * ((DeviceEmulatorOBU)device).getVehicleInfo(); BasicSafetyMessageVerbose bsm =
     * getBasicSafetyMessageVerbose(vehicle); msgCount = (short) (++msgCount % 128); WSMRequest
     * message = new WSMRequest(); message.setPeerMACAddress(WaveShortMessageSAP.BROADCAST_MAC);
     * message.setData(bsm); WSMConfirm confirm = device.getWaveShortMessageSAP().send(message); if
     * (WSMConfirm.WSM_CONFIRM_ACCEPTED != confirm) { LOGGER.log(Level.WARNING,
     * "{}  could not send message.", BSMVerboseProducerApp.class.getSimpleName()); } }
     */

    @Override
    public void init(String[] appConfigs) {
        frequency = Double.parseDouble(appConfigs[0]);
    }

    @Override
    public void performUpdate(OBUDevice device, Object[] messages, Collection<BasicMessage> receive, Double simtime, AppLogger logger) {
        if (lastTxTime != null) {
            if ((simtime - lastTxTime) < frequency) {
                return;
            }
        }
        ObjectFactory factory = new ObjectFactory();
        lastTxTime = simtime;

        boolean doPart2 = false;
        for (Object msg : messages) {
            if (msg instanceof CommonSafetyRequest) {
                doPart2 = true;
            }
        }

        List<BasicMessage> ret = new ArrayList<BasicMessage>(1);
        try {
            BasicSafetyMessageVerbose bsmv = factory.createBasicSafetyMessageVerbose();
            int size = getBasicSafetyMessageVerbose(simtime, device.getVehicleInfo(), doPart2, msgCount, bsmv);
            ret.add(new DSRCMessage(bsmv, DSRCChannel.CH184, WaveMessage.MACBROADCAST, size));
        }
        catch (UnsupportedEncodingException ex) {
            logger.log("Error", ex.toString());
        }
        device.addAppMessages(ret);
    }

    /**
     * Gets a basic safety message for the given vehicle.
     * 
     * @param simtime
     * @param vehicle The vehicle for which to create a BasicSafetyMessage.
     * @param extended True to do extended BSM, false to do normal part I BSM.
     * @return A BasicSafetyMessage representing the given vehicle.
     */
    static int getBasicSafetyMessageVerbose(Double simtime, IVehicle vehicle, boolean extended, short msgCount, BasicSafetyMessageVerbose bsmv) throws UnsupportedEncodingException {
        // temp counter for msg byte size. Part 1 is 42 bytes
        Double tempSize = Double.valueOf(SIZE_PART_1);
        if (bsmv == null) {
            // Initialize the data that doesn't change
            bsmv = new BasicSafetyMessageVerbose();
            bsmv.setMsgID(DSRCMessageID.MSG_ID_BSM);

            bsmv.setSecMark(65535);

            bsmv.setElev(new byte[] { (byte)0xF0, 0 }); // encode 0xF000 or
            // "unknown"
            bsmv.setAccuracy(new byte[] { 0, 0, 0, 0 });

            // -- basic VehicleBasic,
            // -- size VehicleSize, -x- 3 bytes - to be the vehicle width and
            // the vehicle length (in that order) measured in centimeters
            VehicleSize vehicleSize = new VehicleSize();
            vehicleSize.setWidth((int)vehicle.getWidth());
            vehicleSize.setLength((int)vehicle.getLength());
            bsmv.setSize(vehicleSize);

            ByteBuffer id4Bytes = ByteBuffer.allocate(4);
            bsmv.setId(id4Bytes.putInt(vehicle.getVehicleID()).array());

            // -- angle SteeringWheelAngle -x- 1 bytes
            double steeringAngle = vehicle.getSteeringAngle() * 0.667;
            steeringAngle = (steeringAngle > 126.0) ? 126.0 : steeringAngle;
            steeringAngle = (steeringAngle < -126.0) ? -126.0 : steeringAngle;
            bsmv.setAngle(new byte[] { (byte)Math.round(steeringAngle) });

            ByteBuffer brakes2Bytes = ByteBuffer.allocate(2);
            // -- control Control,
            // -- brakes BrakeSystemStatus, -x- 2 bytes

            /*
             * We are assuming that any vehicle with the break pressed has the break engaged for all
             * wheels. This creates a bit string of 1111 (i.e., 15).
             */
            bsmv.setBrakes(brakes2Bytes.putShort((short)(vehicle.isBrakePressed() ? 15 : 0)).array());
        }

        // Create and fill in blob1 as specified in comments below:
        //
        // -- Sent as a single octet blob
        // blob1 BSMblob,
        // --
        // -- The blob consists of the following 38 packed bytes:
        ByteBuffer accelSet7Bytes = ByteBuffer.allocate(7);
        ByteBuffer speed2Bytes = ByteBuffer.allocate(2);
        bsmv.setMsgCnt(msgCount);

        bsmv.setLat(UtilsUnitConversion.convertToOneTenthMicrodegree(vehicle.getLatitude())); // convert
                                                                                              // to
                                                                                              // 1/10th
                                                                                              // microdegree
                                                                                              // to
                                                                                              // meet
                                                                                              // spec
        bsmv.setLong(UtilsUnitConversion.convertToOneTenthMicrodegree(vehicle.getLongitude()));

        int transmissionAndSpeed = 1;
        transmissionAndSpeed <<= 14; // set transmission to 2 Note: TEXAS does
        // not appear to support any vehicle
        // transmission state other than forward
        // gears
        // Note: ablatt - converting feet per second to meters per second is the
        // same operation as converting feet to meters
        double speed = vehicle.getSpeed();
        speed /= 0.02; // convert speed from meters per second to (0.02)meters
        // per second
        // Note: 8191 == speed unavailable
        if (speed >= 8191) {
            speed = 8191;
        }
        else if (speed < 0) {
            speed = 8191;
        }
        transmissionAndSpeed |= ((int)speed);

        bsmv.setSpeed(speed2Bytes.putShort((short)transmissionAndSpeed).array());
        bsmv.setHeading(UtilsUnitConversion.convertDegreesToBSMHeading(vehicle.getHeading()));
        accelSet7Bytes.putShort((short)vehicle.getAcceleration());
        accelSet7Bytes.put((byte)0x07); // 0x07D1 = 2001 = "Unavailable" for
                                        // acceleration
        accelSet7Bytes.put((byte)0xD1);
        accelSet7Bytes.put((byte)-127); // -127 is "Unavailable" for vertical
        // acceleration
        accelSet7Bytes.put((byte)0);
        accelSet7Bytes.put((byte)0);
        bsmv.setAccelSet(accelSet7Bytes.array());

        // Add extended data if CSR received. This is the verbose part.
        if (extended) {
            tempSize += UtilsProducer.addExtendedData(simtime, bsmv, vehicle);
        }
        // Round size to nearest whole byte
        int size = (int)Math.ceil(tempSize);
        return size;
    }

    @Override
    public void appShutdown(AppLogger logger) {}

    @Override
    public String getAppName() {
        return APP_NAME_BSM_VERBOSE_PRODUCER_APP;
    }
}