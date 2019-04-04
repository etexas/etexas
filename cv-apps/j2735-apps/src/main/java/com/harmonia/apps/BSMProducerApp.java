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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.ASN1OutputStream;
import org.bouncycastle.asn1.DEREnumerated;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERTaggedObject;
import org.etexascode.apps.BitUtils;
import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.MsgByteCount;
import org.etexascode.apps.OBUDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.FormattedDSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.CommonSafetyRequest;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.wavesim.WaveMessage;

/**
 * A BSM producing Connected Vehicle App.
 * 
 * @author bbadillo
 * @author ablatt
 * @author ttevendale
 */
public class BSMProducerApp implements IOBUBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_BSM_PRODUCER_APP = "BSMProducerApp";

    /**
     * Size of message in bytes. This is always 39 bytes for basic safety message.
     */
    private static final double SIZE_PART_1 = Math.ceil(MsgByteCount.DSRCMSGID.getByteCount() + MsgByteCount.BSMBLOB.getByteCount());

    /**
     * BSM dsrc message id (is always the same).
     */
    public static final DERTaggedObject DSRCmsgID = new DERTaggedObject(false, 0, new DEREnumerated(2)); // permanent
                                                                                                         // BSM
                                                                                                         // Id

    /**
     * Gets a basic safety message for the given vehicle.
     * 
     * @param simtime
     * @param vehicle The vehicle information.
     * @param extended True to do extended BSM, false to do normal BSM.
     * @return A BasicSafetyMessage representing the given vehicle.
     */
    static int getBasicSafetyMessage(Double simtime, IVehicle vehicle, boolean extended, short msgCount, BasicSafetyMessage bsm) throws UnsupportedEncodingException {
        Double tempSize = Double.valueOf(SIZE_PART_1);
        bsm.setMsgID(DSRCMessageID.MSG_ID_BSM);
        bsm.setStatus(null);
        bsm.setBlob1(buildBSMBlob(vehicle, msgCount));

        // Add extended data if CSR received.
        if (extended) {
            tempSize += UtilsProducer.addExtendedData(simtime, bsm, vehicle);
        }
        int size = (int)Math.ceil(tempSize);
        return size;
    }

    /**
     * Build the BSM blob object from a Vehicle.
     * 
     * @param vehicle The Vehicle to build into the blob.
     * @param msgCount The message count to be used.
     * @return The bsm blob.
     */
    static byte[] buildBSMBlob(IVehicle vehicle, short msgCount) {
        // Create and fill in blob1 as specified in comments below:
        //
        // -- Sent as a single octet blob
        // blob1 BSMblob,
        // --
        // -- The blob consists of the following 38 packed bytes:
        ByteBuffer blob1 = ByteBuffer.allocate(38);
        // -- msgCnt MsgCount, -x- 1 byte
        blob1.put((byte)msgCount);
        // -- id TemporaryID, -x- 4 bytes
        blob1.putInt(vehicle.getVehicleID());
        // -- secMark DSecond, -x- 2 bytes
        blob1.putShort((short)65535);
        // -- pos PositionLocal3D,
        // -- lat Latitude, -x- 4 bytes
        blob1.putInt(UtilsUnitConversion.convertToOneTenthMicrodegree(vehicle.getLatitude()));
        // -- long Longitude, -x- 4 bytes
        blob1.putInt(UtilsUnitConversion.convertToOneTenthMicrodegree(vehicle.getLongitude()));
        // -- elev Elevation, -x- 2 bytes
        blob1.putShort((short)0);
        // -- accuracy PositionalAccuracy, -x- 4 bytes
        blob1.putInt(0);
        // -- motion Motion,
        // -- speed TransmissionAndSpeed, -x- 2 bytes
        int transmissionAndSpeed = 1;
        transmissionAndSpeed <<= 14; // set transmission to 2 Note: TEXAS does
        // not appear to support any vehicle
        // transmission state other than forward
        // gears
        // Note: ablatt - converting feet per second to meters per second is the
        // same operaiton as converting feet to meters
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
        transmissionAndSpeed |= (int)speed;
        blob1.putShort((short)transmissionAndSpeed);
        // -- heading Heading, -x- 2 byte
        blob1.putShort((short)UtilsUnitConversion.convertDegreesToBSMHeading(vehicle.getHeading()));

        // -- angle SteeringWheelAngle -x- 1 bytes
        double steeringAngle = vehicle.getSteeringAngle() * 0.667;
        steeringAngle = (steeringAngle > 126.0) ? 126.0 : steeringAngle;
        steeringAngle = (steeringAngle < -126.0) ? -126.0 : steeringAngle;
        blob1.put((byte)Math.round(steeringAngle));

        // -- accelSet AccelerationSet4Way, -x- 7 bytes
        blob1.putShort((short)vehicle.getAcceleration());
        blob1.putShort((short)0x07D1); // 0x07D1 = 2001 = "Unavailable" for
        // acceleration
        blob1.put((byte)-127); // -127 is "Unavailable" for vertical
        // acceleration
        blob1.put((byte)0);
        blob1.put((byte)0);
        // -- control Control,
        // -- brakes BrakeSystemStatus, -x- 2 bytes

        /*
         * We are assuming that any vehicle with the break pressed has the break engaged for all
         * wheels. This creates a bit string of 1111 (i.e., 15).
         */
        blob1.putShort((short)(vehicle.isBrakePressed() ? 15 : 0));

        int wid = (int)vehicle.getWidth();
        int len = (int)vehicle.getLength();
        ByteBuffer vehicleSize = BitUtils.getVehicleSizeBytes(wid, len, 10, 14);
        blob1.put(vehicleSize);

        return blob1.array();

    }

    /**
     * Build the BSM blob DER object from a Vehicle.
     * 
     * @param vehicle The Vehicle to build into the blob.
     * @param msgCount The message count to be used.
     * @return The blob in DER object form.
     */
    ASN1Encodable buildDERTaggedBSM(IVehicle vehicle, short msgCount) {
        return new DERTaggedObject(false, 1, new DEROctetString(buildBSMBlob(vehicle, msgCount)));
    }

    /**
     * Convert a Vehicle and message count into a DER object representing a BSM.
     * 
     * @param vehicle The vehicle to build off of.
     * @param msgCount The message count being used.
     * @return The BSM in DER object form.
     */
    ASN1Encodable convertToBSM(IVehicle vehicle, short msgCount) {
        return new DERSequence(new ASN1Encodable[] { DSRCmsgID, buildDERTaggedBSM(vehicle, msgCount) });
    }

    /**
     * Build a binary BSM out of a vehicle.
     * 
     * @param vehicle The vehicle to parse.
     * @param msgCount The message count to use.
     * @return The BSM as a byte[].
     */
    public byte[] buildBSM(IVehicle vehicle, short msgCount) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ASN1OutputStream out = new ASN1OutputStream(baos);
        byte[] ret = null;

        try {
            out.writeObject(convertToBSM(vehicle, msgCount));
            out.flush();
            ret = baos.toByteArray();
            out.close();
        }
        catch (IOException e) {
            throw new RuntimeException("fatal error occurred building BSM message", e);
        }

        return ret;
    }

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * An incremented counter for the current number of generated messages (modulo 128).
     */
    private short msgCount = 0;

    @AppConfigProperty(value = "false", description = "The indication of whether or not to include the formatted message data.")
    boolean hasFormattedData;

    /**
     * The frequency at which to produce messages in seconds. SPEC states that 10Hz is the default.
     */
    @AppConfigProperty(value = "0.1", displayName = "frequency", description = "The frequency to send messages in seconds.")
    double frequency;

    @Override
    public void init(String[] appConfigs) {
        hasFormattedData = Boolean.parseBoolean(appConfigs[0]);
        frequency = Double.parseDouble(appConfigs[1]);
    }

    @Override
    public void performUpdate(OBUDevice device, Object[] messages, Collection<BasicMessage> receive, Double simtime, AppLogger logger) {
        if (lastTxTime != null) {
            if (simtime - lastTxTime < frequency) {
                return;
            }
        }

        lastTxTime = simtime;

        boolean doPart2 = false;
        for (Object msg : messages) {
            if (msg instanceof CommonSafetyRequest) {
                doPart2 = true;
            }
        }

        List<BasicMessage> ret = new ArrayList<BasicMessage>(1);
        try {
            BasicSafetyMessage bsm = new BasicSafetyMessage();
            int size = getBasicSafetyMessage(simtime, device.getVehicleInfo(), doPart2, msgCount, bsm);
            byte[] bsmBytes = buildBSM(device.getVehicleInfo(), msgCount);

            if (hasFormattedData) {
                ret.add(new FormattedDSRCMessage(bsmBytes, bsm, DSRCChannel.CH184, WaveMessage.MACBROADCAST, size));
            }
            else {
                ret.add(new DSRCMessage(bsmBytes, DSRCChannel.CH184, WaveMessage.MACBROADCAST, size));
            }
            msgCount = (short)((msgCount + 1) % 128);
        }
        catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
        // the below code exists for debugging purposes only
        // specifically when unexplained errors start to appear
        // especially if you believe those errors might be xml related
        // please remember to comment it out before committing
        /*
         * try { ByteArrayOutputStream stream = new ByteArrayOutputStream(); JAXBContext context =
         * JAXBContext.newInstance(BasicSafetyMessage.class); Marshaller marsh =
         * context.createMarshaller(); marsh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
         * marsh.marshal((BasicSafetyMessage)ret.get(0).getMessage(), stream); String temp = new
         * String(stream.toByteArray()); logger.log("xml", temp); } catch (JAXBException e) { throw
         * new RuntimeException(e); }
         */
        device.addAppMessages(ret);
    }

    @Override
    public void appShutdown(AppLogger logger) {}

    @Override
    public String getAppName() {
        return APP_NAME_BSM_PRODUCER_APP;
    }

}