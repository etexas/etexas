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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.CoberturaIgnore;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.RSEDevice;
import org.etexascode.devicedata.AppConfigProperty;
import org.etexascode.devicedata.AppLogger;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.DSRCChannel;
import org.etexascode.devicedata.DSRCMessage;
import org.etexascode.devicedata.IAppLifecycle;
import org.etexascode.devicedata.IAppName;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.ISignalManager;
import org.etexascode.j2735_2016.elements.DSRCmsgID;
import org.etexascode.j2735_2016.elements.DSecond;
import org.etexascode.j2735_2016.elements.IntersectionStatusObject;
import org.etexascode.j2735_2016.elements.MinuteOfTheYear;
import org.etexascode.j2735_2016.elements.MovementPhaseState.MovementPhase;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.frames.IntersectionReferenceID;
import org.etexascode.j2735_2016.frames.IntersectionState;
import org.etexascode.j2735_2016.frames.IntersectionStateList;
import org.etexascode.j2735_2016.frames.MovementEvent;
import org.etexascode.j2735_2016.frames.MovementEventList;
import org.etexascode.j2735_2016.frames.MovementList;
import org.etexascode.j2735_2016.frames.MovementState;
import org.etexascode.j2735_2016.frames.TimeChangeDetails;
import org.etexascode.j2735_2016.messages.MessageFrame;
import org.etexascode.j2735_2016.messages.SPAT;
import org.etexascode.wavesim.WaveMessage;

/**
 * A SPAT producing Connected Vehicle App.
 * 
 * @author ttevendale
 */
public class SPATProducer2016App implements IRSEBaseApp, IAppLifecycle, IAppName {

    /**
     * The application identifier.
     */
    public static final String APP_NAME_SPAT_PRODUCER_2016_APP = "SPATProducer2016App";

    /**
     * The last recorded time a transmission was made.
     */
    private Double lastTxTime;

    /**
     * An incremented counter for the current number of generated messages (modulo 128).
     */
    private short msgCount = 0;

    /**
     * The frequency at which to produce messages in seconds. No recommended default in SPEC.
     */
    @AppConfigProperty(value = "1.0", description = "The frequency to send messages in seconds.")
    private double frequency;

    /**
     * Builds a signal phase and timing message byte[].
     * 
     * @param signalManagers The signal managers for the execution.
     * @param simTime The current simulation time.
     * @return The SPAT as a byte[].
     */
    private byte[] buildSPAT(Map<Integer, ISignalManager> signalManagers, double simTime) {

        IntersectionStateList intersections = new IntersectionStateList(signalManagers.size());
        int index = 0;

        for (Entry<Integer, ISignalManager> signalManager : signalManagers.entrySet()) {

            intersections.getIntersectionStateArray()[index] = buildIntersection(signalManager.getKey(), signalManager.getValue(), simTime);
            index++;
        }

        SPAT spat = new SPAT(intersections);
        spat.setTimeStamp(buildTimeStamp(simTime));

        MessageFrame frame = new MessageFrame(DSRCmsgID.SPAT, spat.encodeUPER());
        byte[] spatBytes = null;

        try {

            spatBytes = frame.encodeHexUPER().getBytes("UTF-8");
        }
        catch (UnsupportedEncodingException e) {

            throw new IllegalStateException("Unsupported Encoding Exception was thrown while creating SPAT.");
        }

        return spatBytes;
    }

    /**
     * Builds the time stamp for a SPAT message. NOTE: converts seconds to minutes.
     * 
     * @param simTime The simulation time to convert to a time stamp.
     * @return The time stamp.
     */
    private MinuteOfTheYear buildTimeStamp(double simTime) {

        // converts seconds to minutes
        return new MinuteOfTheYear((int)simTime / 60);
    }

    /**
     * Builds an intersection for the SPAT message.
     * 
     * @param intersectionId The intersection ID.
     * @param signalManager The signal manager for the current intersection.
     * @param simTime The current simulation time.
     * @return The intersection.
     */
    private IntersectionState buildIntersection(Integer intersectionId, ISignalManager signalManager, double simTime) {

        IntersectionState intersection = new IntersectionState(new IntersectionReferenceID(intersectionId), new MsgCount(msgCount), buildStatus(), buildStates(signalManager));
        intersection.setTimeStamp(buildIntersectionTimeStamp(simTime));
        return intersection;
    }

    /**
     * Builds the status for the SPAT message.
     * 
     * @return The status.
     */
    private IntersectionStatusObject buildStatus() {

        IntersectionStatusObject status = new IntersectionStatusObject();
        status.setManualControlEnabled(true);
        return status;
    }

    /**
     * Builds the states for the SPAT message.
     * 
     * @param signalManager The signal manager for the current intersection.
     * @return The states.
     */
    private MovementList buildStates(ISignalManager signalManager) {

        ArrayList<MovementState> movementArrayList = new ArrayList<MovementState>();
        for (ISignalIndication signal : signalManager) {

            movementArrayList.add(buildMovementState(signal));
        }

        MovementList movements = new MovementList(movementArrayList.size());
        for (int i = 0; i < movementArrayList.size(); i++) {

            movements.getMovementArray()[i] = movementArrayList.get(i);
        }

        return movements;
    }

    /**
     * Builds a movement state for the SPAT message.
     * 
     * @param signal The signal to build the movement state with.
     * @return A movement state.
     */
    private MovementState buildMovementState(ISignalIndication signal) {

        MovementEventList events = new MovementEventList(1);
        events.getMovementEventArray()[0] = buildEventState(signal);

        return new MovementState(signal.getLaneId(), events);
    }

    /**
     * Builds a movement event for the SPAT message.
     * 
     * @param signal The signal to build the movement event with.
     * @return A movement event.
     */
    private MovementEvent buildEventState(ISignalIndication signal) {

        MovementEvent eventState = new MovementEvent(getEventPhase(signal));
        eventState.setTiming(new TimeChangeDetails((int)signal.getTimeToChange()));

        return eventState;
    }

    /**
     * Gets the event phase based on the signal. NOTE: In the specification it points out that in
     * the US, the Ball type = permissive and the Arrow type = protected.
     * 
     * @param signal The signal to make the event phase from.
     * @return The event phase.
     */
    private MovementPhase getEventPhase(ISignalIndication signal) {

        Type type = signal.getTypeIndication();
        Color color = signal.getColorIndication();
        State state = signal.getStateIndication();
        MovementPhase eventPhase = MovementPhase.UNAVAILABLE;

        switch (type) {

            case BALL:
            case LEFT_ARROW:
            case RIGHT_ARROW:
            case STRAIGHT_ARROW:
            case UTURN_ARROW:
                break;
            default:
                return eventPhase;
        }

        switch (color) {

            case RED:
                switch (state) {

                    case FLASHING:
                        eventPhase = MovementPhase.STOP_THEN_PROCEED;
                        break;
                    case STEADY:
                        eventPhase = MovementPhase.STOP_AND_REMAIN;
                        break;
                    default:
                        break;
                }
                break;
            case YELLOW:
                switch (state) {

                    case FLASHING:
                        eventPhase = MovementPhase.CAUTION_CONFLICTING_TRAFFIC;
                        break;
                    case STEADY:
                        switch (type) {

                            case BALL:
                                eventPhase = MovementPhase.PERMISSIVE_CLEARANCE;
                                break;
                            case LEFT_ARROW:
                            case RIGHT_ARROW:
                            case STRAIGHT_ARROW:
                            case UTURN_ARROW:
                                eventPhase = MovementPhase.PROTECTED_CLEARANCE;
                                break;
                            default:
                                break;
                        }
                        break;
                    default:
                        break;
                }
                break;
            case GREEN:
                switch (type) {

                    case BALL:
                        eventPhase = MovementPhase.PERMISSIVE_MOVEMENT_ALLOWED;
                        break;
                    case LEFT_ARROW:
                    case RIGHT_ARROW:
                    case STRAIGHT_ARROW:
                    case UTURN_ARROW:
                        eventPhase = MovementPhase.PROTECTED_MOVEMENT_ALLOWED;
                        break;
                    default:
                        break;
                }
                break;
            default:
                break;
        }

        return eventPhase;
    }

    /**
     * Builds the second mark part for the message. NOTE: Converts from seconds (simulation time) to
     * milliseconds and ensures that the value is within a minute.
     * 
     * @param simTime The simulation time to calculate the second mark with.
     * @return The second mark.
     */
    private DSecond buildIntersectionTimeStamp(double simTime) {

        // constrains to a minute
        double timeStamp = simTime % 60;

        // converts seconds to milliseconds
        return new DSecond((int)(timeStamp * 1000));
    }

    @Override
    public void init(String[] appConfigs) {

        frequency = Double.parseDouble(appConfigs[0]);
    }

    @Override
    public void performUpdate(RSEDevice device, Object[] messages, Collection<BasicMessage> receive, Double simTime, AppLogger logger) {

        if (lastTxTime != null && simTime - lastTxTime < frequency) {

            return;
        }

        lastTxTime = simTime;

        List<BasicMessage> ret = new ArrayList<BasicMessage>();

        byte[] spat = buildSPAT(device.getSignalManagers(), simTime);
        ret.add(new DSRCMessage(spat, DSRCChannel.CH184, WaveMessage.MACBROADCAST, spat.length));

        msgCount = (short)(++msgCount % 128);

        device.addAppMessages(ret);
    }

    @Override
    @CoberturaIgnore
    public void appShutdown(AppLogger logger) {

        logger.log(APP_NAME_SPAT_PRODUCER_2016_APP, "The application has shutdown.");
    }

    @Override
    public String getAppName() {

        return APP_NAME_SPAT_PRODUCER_2016_APP;
    }
}