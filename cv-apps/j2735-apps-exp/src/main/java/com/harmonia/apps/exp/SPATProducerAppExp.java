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
package com.harmonia.apps.exp;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.LaneMovement.Movement;
import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneManager;
import org.etexascode.interrep.datamodel.interfaces.ILaneMovement;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.j2735.exp.IntersectionStateExp;
import org.etexascode.j2735.exp.MovementStateExp;
import org.etexascode.j2735.exp.ObjectFactoryExp;
import org.etexascode.j2735.exp.SPATExp;
import org.etexascode.j2735.util.SignalLightState;
import org.etexascode.wavesim.WaveMessage;

/**
 * SPAT producer app.
 * 
 * @author jrutherford
 * @author ablatt
 */
public class SPATProducerAppExp implements IRSEBaseApp, IAppLifecycle, IAppName {

    /**
     * Identifier for this App.
     */
    public static final String APP_NAME_SPAT_PRODUCER = "SPAT-producer-exp";

    /** The last transmission time. */
    private Double lastTxTime;

    /**
     * An incremented counter for the current number of generated messages (modulo 128).
     */
    private short msgCount = 0;

    /** The frequency at which to produce messages. */
    @AppConfigProperty(value = "1.0")
    double frequency;

    /**
     * Get the Signal Phase and Timing information from the simulation program.
     * 
     * @return An object instance of the SPAT class which was automatically generated from the J2735
     * Message Set Dictionary.
     */
    /**
     * Get the Signal Phase and Timing information from the simulation program.
     * 
     * @param signalManager The current signal manager to build a SPAT out of.
     * @param laneManager A lane manager for cross referencing what cars can actually do given to
     * current signal.
     * @param simTime Brought in because SPAT requires the time at which the message is to be sent.
     * @return An object instance of the SPAT class which was automatically generated from the J2735
     * Message Set Dictionary.
     */
    public static SPATExp getCurrentSPATMessage(ISignalManager signalManager, ILaneManager laneManager, double simTime) {
        // Create the object factory.
        ObjectFactoryExp factory = new ObjectFactoryExp();

        IntersectionStateExp IntersectionStateExp = factory.createIntersectionStateExp();

        IntersectionStateExp.getId().setPayload(getIntersectId(laneManager));
        IntersectionStateExp.getStatus().setPayload(new byte[] { (byte)0x00 });
        IntersectionStateExp.getTimeStamp().setPayload(getTimeStamp(simTime));

        List<IndsMeaning> meanings = reduceMeanings(buildMeanings(getSignalIndicationsByLaneId(signalManager)));
        List<MovementStateExp> states = IntersectionStateExp.getStates().getMovementState();

        for (IndsMeaning im : meanings) {
            states.add(getMoveStateFromIndsMeaning(im, laneManager, factory));
        }

        SPATExp ret = factory.createSPATExp();
        ret.setSpatBlob(IntersectionStateExp);
        ret.setSize((short)0); // TODO: ablatt - this will need to be
        // calculated once SPAT blob is being properly
        // constructed
        return ret;
    }

    /**
     * Get a formatted (for the SPAT message) version of the intersection identifier.
     * 
     * @param lmi The lane manager.
     * @return Gets the intersection ID.
     */
    static byte[] getIntersectId(ILaneManager lmi) {
        ByteBuffer bb = ByteBuffer.allocate(4);
        bb.putInt(lmi.getIntersectionId());
        return bb.array();
    }

    /**
     * Format the sim time into the form that is used by the SPAT message.
     * 
     * @param simTime the sim time provided by the system.
     * @return The time stamp.
     */
    static byte[] getTimeStamp(double simTime) {
        ByteBuffer bb = ByteBuffer.allocate(5);

        bb.putInt((int)simTime);
        bb.put((byte)((simTime * 10) % 10));

        return bb.array();
    }

    /**
     * Filter signal indications into a map keyed on lane ids.
     * 
     * @param inds The iterable signal indication object.
     * @return The signal indications in the lane.
     */
    static Map<Integer, List<ISignalIndication>> getSignalIndicationsByLaneId(Iterable<ISignalIndication> inds) {
        Map<Integer, List<ISignalIndication>> ret = new HashMap<Integer, List<ISignalIndication>>();
        for (ISignalIndication si : inds) {
            if (!ret.containsKey(si.getLaneId())) {
                ret.put(si.getLaneId(), new ArrayList<ISignalIndication>(4));
            }

            ret.get(si.getLaneId()).add(si);
        }

        return ret;
    }

    /**
     * Convert the signal indications for a lane into what those indications generally mean for that
     * lane.
     * 
     * @param indicationsByLaneId Key: Lane id, Value: The list of signal indications which apply to
     * that lane.
     * @return The meaning of the indications with respect to that lane.
     */
    static List<IndsMeaning> buildMeanings(Map<Integer, List<ISignalIndication>> indicationsByLaneId) {
        List<IndsMeaning> ret = new ArrayList<IndsMeaning>(indicationsByLaneId.size());
        for (Entry<Integer, List<ISignalIndication>> i : indicationsByLaneId.entrySet()) {
            ret.add(new IndsMeaning(i.getValue(), i.getKey()));
        }

        return ret;
    }

    /**
     * Combine the lanes which have identical indications meanings.
     * 
     * @param means The original list of indications.
     * @return The list of combined indications.
     */
    static List<IndsMeaning> reduceMeanings(List<IndsMeaning> means) {
        List<IndsMeaning> ret = new ArrayList<IndsMeaning>(means.size());

        while (means.size() > 0) {
            IndsMeaning im = means.remove(0);
            for (int i = 0; i < means.size(); i++) {
                IndsMeaning im2 = means.get(i);
                if (im.canCombineWith(im2)) {
                    im.combine(im2);
                    means.remove(im2);
                    i--;
                }
            }

            ret.add(im);
        }

        return ret;
    }

    /**
     * Build a movement state object (xml) out of the meaning of some indications.
     * 
     * @param im The indication state which we are building the light state out of
     * @param lmi For determining valid directions a lane could go
     * @param factory For creating new instances of objects
     * @return The movement state corresponding to the xml.
     */
    static MovementStateExp getMoveStateFromIndsMeaning(IndsMeaning im, ILaneManager lmi, ObjectFactoryExp factory) {
        MovementStateExp movementState = factory.createMovementState();

        movementState.getLaneSet().setPayload(getLaneSet(im, lmi));
        movementState.getCurrState().setPayload(getCurrState(im));
        movementState.getMinTimeRemaining().setPayload(getMinTimeRemaining(im));
        movementState.getMaxTimeRemaining().setPayload(getMaxTimeRemaining(im));

        return movementState;
    }

    /**
     * Get the valid movements within the lane set, specified by im, which are also valid according
     * to the lights.
     * 
     * @param im The indication (+ applicable lane set) which we want to format according to the
     * SPAT specification
     * @param lmi The lane manager (contains the valid movements for the lane irrespective of the
     * state of the light).
     * @return The lane set formatted according to the SPAT specification.
     */
    static byte[] getLaneSet(IndsMeaning im, ILaneManager lmi) {
        ByteBuffer bb = ByteBuffer.allocate(im.laneIds.size() * 2);

        for (Integer i : im.laneIds) {
            ILane li = lmi.getLaneById(i);
            byte set = 0x00;

            for (ILaneMovement movement : li.getLaneMovements().values()) {
                Movement move = movement.getMovement();
                if (move == Movement.LEFT_TURN_ON_RED) {
                    set = (byte)(set | ((byte)0x02));
                }
                else if (move == Movement.LEFT_TURN) {
                    for (ISignalIndication sii : im.sis) {
                        if (indicationAllows(sii, SignalIndication.Type.LEFT_ARROW)) {
                            set = (byte)(set | ((byte)0x02));
                            break;
                        }
                    }
                }
                else if (move == Movement.RIGHT_TURN_ON_RED) {
                    set = (byte)(set | ((byte)0x04));
                }
                else if (move == Movement.RIGHT_TURN) {
                    for (ISignalIndication sii : im.sis) {
                        if (indicationAllows(sii, SignalIndication.Type.RIGHT_ARROW)) {
                            set = (byte)(set | ((byte)0x04));
                            break;
                        }
                    }
                }
                else if (move == Movement.U_TURN) {
                    for (ISignalIndication sii : im.sis) {
                        if (indicationAllows(sii, SignalIndication.Type.UTURN_ARROW)) {
                            set = (byte)(set | ((byte)0x08));
                            break;
                        }
                    }
                }
                else if (move == Movement.STRAIGHT) {
                    for (ISignalIndication sii : im.sis) {
                        if (indicationAllows(sii, SignalIndication.Type.STRAIGHT_ARROW)) {
                            set = (byte)(set | ((byte)0x01));
                            break;
                        }
                    }
                }
            }

            bb.put(set);
            bb.put(i.byteValue());
        }

        return bb.array();
    }

    /**
     * Determines if a specific movement (represented by the type) is allowed by the signal
     * indication.
     * 
     * @param sii The signal indication.
     * @param type The movement type
     * @return True if the indication allows the movement type. False otherwise.
     */
    static boolean indicationAllows(ISignalIndication sii, SignalIndication.Type type) {
        return ((type == sii.getTypeIndication()) && ((sii.getColorIndication() == SignalIndication.Color.GREEN) || (sii.getColorIndication() == SignalIndication.Color.YELLOW))
                || (SignalIndication.Type.BALL == sii.getTypeIndication())
                        && ((sii.getColorIndication() == SignalIndication.Color.GREEN) || (sii.getColorIndication() == SignalIndication.Color.YELLOW)));
    }

    /**
     * Convert the indications meaning into a current state compliant with the SPAT specification
     * 
     * @param im The meaning to convert
     * @return The compliant byte array
     */
    static byte[] getCurrState(IndsMeaning im) {
        long signalLightState = 0;
        for (ISignalIndication sii : im.sis) {
            signalLightState = signalLightState | getSignalLightStateFromSignalData(sii);
        }

        ByteBuffer currState = ByteBuffer.allocate(8);
        currState.putLong(signalLightState);
        return chop(currState.array());
    }

    /**
     * Remove preceding zeros from byte[] in.
     * 
     * @param in
     * @return The resulting byte[].
     */
    static byte[] chop(byte[] in) {
        for (int i = 0; i < in.length; i++) {
            if (in[i] != (byte)0) {
                byte[] ret = new byte[in.length - i];
                int j = 0;

                for (; i < in.length; i++) {
                    ret[j] = in[i];
                    j++;
                }

                return ret;
            }
        }

        return new byte[] { (byte)0 };
    }

    /**
     * Get the minimum (only) time remaining for the indication set in im.
     * 
     * @param im The IndsMeaning object.
     * @return The minimum time remaining.
     */
    static byte[] getMinTimeRemaining(IndsMeaning im) {
        ByteBuffer bb = ByteBuffer.allocate(2);
        bb.putShort((short)Math.round(getTime(im.sis)));
        return bb.array();
    }

    /**
     * Get the maximum (only) time remaining for the indication set in im.
     * 
     * @param im The IndsMeaning object.
     * @return The maximum time remaining.
     */
    static byte[] getMaxTimeRemaining(IndsMeaning im) {
        ByteBuffer bb = ByteBuffer.allocate(2);
        bb.putShort((short)Math.round(getTime(im.sis)));
        return bb.array();
    }

    /**
     * Get the smallest time remaining among the list of signal indications. This value indicates
     * the next time that a change will occur in the (insert whatever the list represents here).
     * 
     * @param sis List of signal indications.
     * @return Gets the time.
     */
    static double getTime(List<ISignalIndication> sis) {
        double ret = Double.MAX_VALUE;

        for (ISignalIndication si : sis) {
            if (si.getTimeToChange() < ret) {
                ret = si.getTimeToChange();
            }
        }

        return ret;
    }

    /**
     * Get the J2735 SignalLightState representation using a SIMPRO signal indication
     * 
     * @param signalIndication The signal indication info.
     * @return A long value which represents a bit-wise ORing of signal light states
     */
    static long getSignalLightStateFromSignalData(ISignalIndication signalIndication) {
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
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.BALL_GREEN | SignalLightState.BALL_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.BALL_YELLOW;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.BALL_YELLOW | SignalLightState.BALL_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.BALL_RED;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.BALL_RED | SignalLightState.BALL_FLASHING;
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
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.LEFT_ARROW_GREEN | SignalLightState.LEFT_ARROW_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.LEFT_ARROW_YELLOW;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.LEFT_ARROW_YELLOW | SignalLightState.LEFT_ARROW_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.LEFT_ARROW_RED;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.LEFT_ARROW_RED | SignalLightState.LEFT_ARROW_FLASHING;
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
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.RIGHT_ARROW_GREEN | SignalLightState.RIGHT_ARROW_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.RIGHT_ARROW_YELLOW;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.RIGHT_ARROW_YELLOW | SignalLightState.RIGHT_ARROW_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.RIGHT_ARROW_RED;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.RIGHT_ARROW_RED | SignalLightState.RIGHT_ARROW_FLASHING;
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
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.STRAIGHT_GREEN | SignalLightState.STRAIGHT_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.STRAIGHT_YELLOW;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.STRAIGHT_YELLOW | SignalLightState.STRAIGHT_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.STRAIGHT_RED;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.STRAIGHT_RED | SignalLightState.STRAIGHT_FLASHING;
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
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.UTURN_ARROW_GREEN | SignalLightState.UTURN_ARROW_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case YELLOW:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.UTURN_ARROW_YELLOW;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.UTURN_ARROW_YELLOW | SignalLightState.UTURN_ARROW_FLASHING;
                                break;
                            default:
                                break;
                        }
                        break;
                    case RED:
                        switch (state) {
                            case STEADY:
                                signalLightState = SignalLightState.UTURN_ARROW_RED;
                                break;
                            case FLASHING:
                                signalLightState = SignalLightState.UTURN_ARROW_RED | SignalLightState.UTURN_ARROW_FLASHING;
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

        return signalLightState;
    }

    @Override
    public void init(String[] appConfigs) {
        frequency = Double.parseDouble(appConfigs[0]);
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {
        if (lastTxTime != null) {
            if (simTime - lastTxTime < frequency) {
                return;
            }
        }

        lastTxTime = simTime;
        logger.log("SimTime", simTime.toString());
        ArrayList<BasicMessage> ret = new ArrayList<BasicMessage>();

        for (Entry<Integer, ISignalManager> signalManagerEntry : device.getSignalManagers().entrySet()) {

            SPATExp msg = getCurrentSPATMessage(signalManagerEntry.getValue(), device.getLaneManager(signalManagerEntry.getKey()), simTime);
            msg.setContentVersion((byte)msgCount);
            ret.add(new DSRCMessage(msg, DSRCChannel.CH184, WaveMessage.MACBROADCAST, msg.getSize()));
            msgCount = (short)(++msgCount % 128);
        }

        // code for creating an XML and logging it (good for debugging)
        /*
         * try { ByteArrayOutputStream stream = new ByteArrayOutputStream(); JAXBContext context =
         * JAXBContext.newInstance(SPAT.Intersections.class); Marshaller marsh =
         * context.createMarshaller(); marsh.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
         * marsh.marshal(msg, stream); String temp = new String(stream.toByteArray());
         * logger.log("xml", temp); } catch (JAXBException e) { throw new RuntimeException(e); }
         */

        device.addAppMessages(ret);
    }

    @Override
    public void appShutdown(AppLogger logger) {}

    @Override
    public String getAppName() {
        return APP_NAME_SPAT_PRODUCER;
    }
}