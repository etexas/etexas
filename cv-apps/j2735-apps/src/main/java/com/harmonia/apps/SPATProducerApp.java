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

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import org.bouncycastle.asn1.ASN1Encodable;
import org.bouncycastle.asn1.DEREnumerated;
import org.bouncycastle.asn1.DERInteger;
import org.bouncycastle.asn1.DEROctetString;
import org.bouncycastle.asn1.DERSequence;
import org.bouncycastle.asn1.DERTaggedObject;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.MsgByteCount;
import org.etexascode.apps.RSEDevice;
import org.etexascode.apps.SignalLightStateEnum;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.FormattedDSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;
import org.etexascode.j2735.IntersectionState;
import org.etexascode.j2735.MovementState;
import org.etexascode.j2735.SPAT;
import org.etexascode.j2735.util.DSRCMessageID;
import org.etexascode.j2735.util.SignalLightState;
import org.etexascode.wavesim.WaveMessage;

/**
 * A SPAT producing Connected Vehicle App.
 * 
 * @author bbadillo
 * @author ablatt
 * @author bmauldon
 * @author ttevendale
 */
public class SPATProducerApp implements IRSEBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_SPAT_PRODUCER_APP = "SPATProducerApp";

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * An incremented counter for the current number of generated messages (modulo 128).
     */
    private short msgCount = 0;

    /**
     * SPAT dsrc message id (is always the same).
     */
    public static final ASN1Encodable DSRCmsgID = new DERTaggedObject(false, 0, new DEREnumerated(13)); // permanent
                                                                                                        // SPAT
                                                                                                        // Id

    /**
     * Array used to convert a list into an array (dealing with a type/efficiency mismatch)
     */
    static final ASN1Encodable[] CONVERSION_ARRAY = new ASN1Encodable[0];

    /**
     * Intersection Id being used
     */
    ASN1Encodable intersectionId = null;

    /**
     * Intersection status being used
     */
    ASN1Encodable intersectionStatus = null;

    @AppConfigProperty(value = "false", description = "The indication of whether or not to include the formatted message data.")
    boolean hasFormattedData;

    /**
     * The frequency at which to produce messages in seconds. No recommended default in SPEC.
     */
    @AppConfigProperty(value = "1.0", description = "The frequency to send messages in seconds.")
    double frequency;

    /**
     * Store the object so it doesn't have to be recreated.
     */
    SPAT spatMessage = null;

    /**
     * Get the Signal Phase and Timing information from the simulation program.
     * 
     * @return An object instance of the SPAT class which was automatically generated from the J2735
     * Message Set Dictionary
     */
    /**
     * Creates a formatted SPaT message for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @param signalManager The signal manager for the intersection.
     * @return The size (bytes) of the created message.
     */
    public int getFormattedSPATMessage(Integer intersection, ISignalManager signalManager) {

        Double tempSize = new Double(0.0);

        if (spatMessage == null) {
            // Initialize an intersection in a SPAT message
            spatMessage = new SPAT();
            spatMessage.setMsgID(DSRCMessageID.MSG_ID_SPAT);
            // increment message size
            tempSize += MsgByteCount.DSRCMSGID.getByteCount();
            SPAT.IntersectionStates intersections = new SPAT.IntersectionStates();
            spatMessage.setIntersectionStates(intersections);
            List<IntersectionState> intersectionStates = intersections.getIntersectionState();
            IntersectionState intersectionState = new IntersectionState();
            intersectionStates.add(intersectionState);
            IntersectionState.States states = new IntersectionState.States();
            intersectionState.setStates(states);
            ByteBuffer idBuffer = ByteBuffer.allocate(4);
            idBuffer.putInt(intersection);
            intersectionState.setId(idBuffer.array());
            // increment message size
            tempSize += MsgByteCount.INTERSECTIONID.getByteCount();
            /*
             * Flags represent states we won't have. Clarification from David Kelley: 0 - Manual
             * control is on. Therefore the values provide for phase time are subject to arbitrary
             * changes, think of this as a quality metric 1 - All signal timing has stopped. True
             * for stop/yield signs and when signals have stopped working. Correct but there is
             * great debate on what this mean or if this is also to be used in some way in a dark
             * state. 2 - All signals are flashing 3 - Suggest you think of is as emergency vehicle
             * has been granted preempt. 4 - Used for intersection with priority system for
             * buses/trains Correct, but a limitation of this edition is that 'hidden' movements and
             * phase (those seen only be a sub set of users, typically a transit or rail systems
             * with private signal heads) did not inter-operate with this well, We think this is now
             * corrected. If no conditions are true then set all values to 0.
             */
            intersectionState.setStatus(new byte[] { (byte)0, (byte)0, (byte)0, (byte)0, (byte)0, (byte)0, (byte)0, (byte)0 });
        }
        // increment message size
        tempSize += MsgByteCount.INTERSECTIONSTATUSOBJECT.getByteCount();
        List<MovementState> movementStates = spatMessage.getIntersectionStates().getIntersectionState().get(0).getStates().getMovementState();
        movementStates.clear();
        // Iterate over all SignalIndications
        for (ISignalIndication ind : signalManager) {
            MovementState movementState = new MovementState();
            byte[] laneSet = new byte[1];

            laneSet[0] = (byte)(ind.getLaneId() & 0xFF);
            movementState.setLaneSet(laneSet);
            // increment message size
            tempSize += MsgByteCount.LANESET.getByteCount();
            long signalLightState = 0;

            // Set the signal light state of this lane in the SPAT message
            SignalLightStateClass slsc = convertToSignalState(ind, tempSize);
            signalLightState = signalLightState | slsc.lightState;
            tempSize = slsc.newsize;

            movementState.setTimeToChange((int)UtilsUnitConversion.convertSecondsToOneTenthSeconds(ind.getTimeToChange()));
            // increment message size
            tempSize += MsgByteCount.TIMEMARK.getByteCount();
            movementState.setCurrState(signalLightState);
            // increment message size
            tempSize += MsgByteCount.SIGNALSTATE.getByteCount();
            movementState.setYellState(null);
            // increment message size
            tempSize += MsgByteCount.SIGNALSTATE.getByteCount();
            // Add the information to the SPAT message
            movementStates.add(movementState);

        }
        // round up the message size to the nearest byte
        int size = (int)Math.ceil(tempSize);
        return size;
    }

    /**
     * Parse the lane into an octet string.
     * 
     * @param isi The signal indication which contains the lane id.
     * @return The lane octet string.
     */
    ASN1Encodable parseLaneSet(ISignalIndication isi) {
        return new DEROctetString(new byte[] { (byte)(isi.getLaneId() & 0xFF) });
    }

    /**
     * Parse the current light state into an integer form.
     * 
     * @param isi The signal indication containing the light state.
     * @return An integer representation of the current light state.
     */
    ASN1Encodable parseCurrState(ISignalIndication isi) {
        return new DERInteger(BigInteger.valueOf(convertToSignalState(isi, 0).lightState));
    }

    /**
     * Parse the time to change into integer form.
     * 
     * @param isi The signal indication containing the time to change.
     * @return The integer form of the time to change.
     */
    ASN1Encodable parseTimeMark(ISignalIndication isi) {
        return new DERInteger((int)UtilsUnitConversion.convertSecondsToOneTenthSeconds(isi.getTimeToChange()));
    }

    /**
     * Parse the signal state out of the signal indication and put that into a sequence.
     * 
     * @param isi The signal indication to parse.
     * @return The resulting DER sequence.
     */
    ASN1Encodable parseSigState(ISignalIndication isi) {
        ASN1Encodable[] ret = new ASN1Encodable[] { new DERTaggedObject(false, 2, parseLaneSet(isi)), new DERTaggedObject(false, 3, parseCurrState(isi)),
                new DERTaggedObject(false, 6, parseTimeMark(isi)) };
        return new DERSequence(ret);
    }

    /**
     * Parse the movement set out of the signal manager.
     * 
     * @param sigMan The signal manager to parse.
     * @return The movement set.
     */
    ASN1Encodable parseMovementSet(ISignalManager sigMan) {
        List<ASN1Encodable> ret = new LinkedList<ASN1Encodable>();
        for (ISignalIndication isi : sigMan) {
            ret.add(parseSigState(isi));
        }
        return new DERSequence(ret.toArray(CONVERSION_ARRAY));
    }

    /**
     * Parse an intersection state out of the signal manager.
     * 
     * @param sigMan The signal manager to parse.
     * @return The resulting DER sequence.
     */
    ASN1Encodable getIntersectionState(ISignalManager sigMan) {
        // TODO: ablatt - "debug mode" is supposed to contain 1 additional field
        ASN1Encodable[] ret = new ASN1Encodable[] { intersectionId, intersectionStatus, new DERTaggedObject(false, 5, parseMovementSet(sigMan)) };
        return new DERSequence(ret);
    }

    /**
     * Parse an intersection out of a signal manager.
     * 
     * @param sigMan The signal manager to parse.
     * @return The intersection in DER sequence form.
     */
    ASN1Encodable parseIntersection(ISignalManager sigMan) {
        return new DERSequence(new ASN1Encodable[] { getIntersectionState(sigMan) });
    }

    /**
     * Parse a SPAT message out of a signal manager.
     * 
     * @param sigMan The signal manager to parse.
     * @return The SPAT message in DER object format.
     */
    ASN1Encodable convertToSPAT(ISignalManager sigMan) {
        // TODO: ablatt - "debug mode" is supposed to contain 1 additional field
        ASN1Encodable[] ret = new ASN1Encodable[] { DSRCmsgID, new DERTaggedObject(false, 2, parseIntersection(sigMan)) };
        return new DERSequence(ret);
    }

    /**
     * Build a SPAT message for the specified intersection.
     * 
     * @param intersection The ID of the intersection.
     * @param sigManager The signal manager for the intersection.
     * @return The SPAT message in byte array form.
     */
    public byte[] buildSPAT(Integer intersection, ISignalManager sigManager) {

        ByteBuffer idBuffer = ByteBuffer.allocate(2);
        idBuffer.putShort((short)intersection.intValue());
        intersectionId = new DERTaggedObject(false, 1, new DEROctetString(idBuffer.array()));
        intersectionStatus = new DERTaggedObject(false, 2, new DEROctetString(new byte[] { (byte)0 }));

        return convertToSPAT(sigManager).getDEREncoded();
    }

    /**
     * Get the J2735 SignalLightState representation using a SIMPRO signal indication. Increments
     * the message size.
     * 
     * @param signalIndication The signal indication info.
     * @param tempSize The current running size of the message.
     * @return A tuple containing a long value which represents a bit-wise ORing of signal light
     * states and a double representing the new size of the message when the long is added to the
     * message.
     */
    protected SignalLightStateClass convertToSignalState(ISignalIndication signalIndication, double tempSize) {
        long signalLightState = 0;

        Color color = signalIndication.getColorIndication();
        State state = signalIndication.getStateIndication();
        Type type = signalIndication.getTypeIndication();

        switch (type) {
            case BALL:
                switch (color) {
                    case GREEN:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.BALL_GREEN;
                                tempSize += SignalLightStateEnum.BALLSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.BALL_GREEN | SignalLightState.BALL_FLASHING;
                                tempSize += SignalLightStateEnum.BALLFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.BALL_YELLOW;
                                tempSize += SignalLightStateEnum.BALLSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.BALL_YELLOW | SignalLightState.BALL_FLASHING;
                                tempSize += SignalLightStateEnum.BALLFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.BALL_RED;
                                tempSize += SignalLightStateEnum.BALLSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.BALL_RED | SignalLightState.BALL_FLASHING;
                                tempSize += SignalLightStateEnum.BALLFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case NONE:
                        signalLightState = SignalLightState.DARK;
                        break;
                }
                break;
            case LEFT_ARROW:
                switch (color) {
                    case GREEN:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.LEFT_ARROW_GREEN;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.LEFT_ARROW_GREEN | SignalLightState.LEFT_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.LEFT_ARROW_YELLOW;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.LEFT_ARROW_YELLOW | SignalLightState.LEFT_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.LEFT_ARROW_RED;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.LEFT_ARROW_RED | SignalLightState.LEFT_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case NONE:
                        signalLightState = SignalLightState.DARK;
                        break;
                }
                break;
            case RIGHT_ARROW:
                switch (color) {
                    case GREEN:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.RIGHT_ARROW_GREEN;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.RIGHT_ARROW_GREEN | SignalLightState.RIGHT_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.RIGHT_ARROW_YELLOW;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.RIGHT_ARROW_YELLOW | SignalLightState.RIGHT_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.RIGHT_ARROW_RED;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.RIGHT_ARROW_RED | SignalLightState.RIGHT_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case NONE:
                        signalLightState = SignalLightState.DARK;
                        break;
                }
                break;
            case STRAIGHT_ARROW:
                switch (color) {
                    case GREEN:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.STRAIGHT_GREEN;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.STRAIGHT_GREEN | SignalLightState.STRAIGHT_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.STRAIGHT_YELLOW;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.STRAIGHT_YELLOW | SignalLightState.STRAIGHT_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.STRAIGHT_RED;
                                tempSize += SignalLightStateEnum.ARROWSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.STRAIGHT_RED | SignalLightState.STRAIGHT_FLASHING;
                                tempSize += SignalLightStateEnum.ARROWFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case NONE:
                        signalLightState = SignalLightState.DARK;
                        break;
                }
                break;
            case UTURN_ARROW:
                switch (color) {
                    case GREEN:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.UTURN_ARROW_GREEN;
                                tempSize += SignalLightStateEnum.SOFTARROWUSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.UTURN_ARROW_GREEN | SignalLightState.UTURN_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.SOFTARROWUFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.UTURN_ARROW_YELLOW;
                                tempSize += SignalLightStateEnum.SOFTARROWUSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.UTURN_ARROW_YELLOW | SignalLightState.UTURN_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.SOFTARROWUFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.UTURN_ARROW_RED;
                                tempSize += SignalLightStateEnum.SOFTARROWUSTEADY.getByteCount();
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.UTURN_ARROW_RED | SignalLightState.UTURN_ARROW_FLASHING;
                                tempSize += SignalLightStateEnum.SOFTARROWUFLASHING.getByteCount();
                                break;
                            default:
                                break;
                        }
                        break;
                    case NONE:
                        signalLightState = SignalLightState.DARK;
                        break;
                }
                break;
            case STOP_SIGN:
                signalLightState = SignalLightState.DARK;
                break;
            case YIELD_SIGN:
                signalLightState = SignalLightState.DARK;
                break;
            case UNCONTROLLED:
                signalLightState = SignalLightState.DARK;
                break;
            case UNKNOWN:
                signalLightState = SignalLightState.DARK;
                break;
            // default:
            // break;
        }

        SignalLightStateClass slsc = new SignalLightStateClass();
        slsc.lightState = signalLightState;
        slsc.newsize = tempSize;

        return slsc;
    }

    /**
     * Inner class used for a specialized return value.
     * 
     * @author ablatt
     */
    static class SignalLightStateClass {

        /**
         * The state of the light.
         */
        long lightState;

        /**
         * The new size of the message when this new light state is introduced.
         */
        double newsize;
    }

    @Override
    public void init(String[] appConfigs) {
        hasFormattedData = Boolean.parseBoolean(appConfigs[0]);
        frequency = Double.parseDouble(appConfigs[1]);
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        if (lastTxTime != null) {

            if (simTime - lastTxTime < frequency) {

                return;
            }
        }

        lastTxTime = simTime;
        List<BasicMessage> ret = new ArrayList<BasicMessage>();

        for (Entry<Integer, ISignalManager> signalManager : device.getSignalManagers().entrySet()) {

            byte[] spat = buildSPAT(signalManager.getKey(), signalManager.getValue());

            if (hasFormattedData) {

                getFormattedSPATMessage(signalManager.getKey(), signalManager.getValue());
                ret.add(new FormattedDSRCMessage(spat, spatMessage, DSRCChannel.CH184, WaveMessage.MACBROADCAST, spat.length));
            }
            else {

                ret.add(new DSRCMessage(spat, DSRCChannel.CH184, WaveMessage.MACBROADCAST, spat.length));
            }

            msgCount = (short)(++msgCount % 128);
        }

        device.addAppMessages(ret);
    }

    @Override
    public void appShutdown(AppLogger logger) {}

    @Override
    public String getAppName() {
        return APP_NAME_SPAT_PRODUCER_APP;
    }
}