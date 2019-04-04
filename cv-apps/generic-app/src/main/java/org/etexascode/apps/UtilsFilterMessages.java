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
package org.etexascode.apps;

import java.util.ArrayList;
import java.util.Collection;

import org.etexascode.j2735.BasicSafetyMessage;
import org.etexascode.j2735.BasicSafetyMessageVerbose;
import org.etexascode.j2735.MapData;
import org.etexascode.j2735.SPAT;

/**
 * A class built to filter a list of messages into a list of specific messages. Contains a
 * generalized Object[] filter if you would like.
 * 
 * @author ablatt
 */
public class UtilsFilterMessages {

    /**
     * Filters incoming messages to return a list of BasicSafetyMessage.
     * 
     * @param messages The messages.
     * @return The collection of messages.
     */
    public static Collection<BasicSafetyMessage> filterForBSM(Object[] messages) {
        return filterMessages(BasicSafetyMessage.class, messages);
    }

    /**
     * Filters incoming messages to return a list of BasicSafetyMessageVerbose.
     * 
     * @param messages The messages.
     * @return The collection of messages.
     */
    public static Collection<BasicSafetyMessageVerbose> filterForBSMV(Object[] messages) {
        return filterMessages(BasicSafetyMessageVerbose.class, messages);
    }

    /**
     * Filters incoming messages to return a list of SPAT.
     * 
     * @param messages The messages.
     * @return The collection of messages.
     */
    public static Collection<SPAT> filterForSPAT(Object[] messages) {
        return filterMessages(SPAT.class, messages);
    }

    /**
     * Filters incoming messages to return a list of MapData.
     * 
     * @param messages The messages.
     * @return The collection of messages.
     */
    public static Collection<MapData> filterForMap(Object[] messages) {
        return filterMessages(MapData.class, messages);
    }

    /**
     * A generalized filter method which takes in an arbitrary object array and returns a list of
     * objects of class type c.
     * 
     * @param <E> The type.
     * @param c The class type to filter.
     * @param messages The list of objects to filter.
     * @return The list of objects of type c in list messages.
     */
    public static <E> Collection<E> filterMessages(Class<E> c, Object[] messages) {
        Collection<E> ret = new ArrayList<E>(messages.length);

        for (Object o : messages) {
            if (o.getClass().isAssignableFrom(c)) {
                ret.add((E)o);
            }
        }

        return ret;
    }
}
