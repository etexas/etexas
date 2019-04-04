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
package org.etexascode.apps.hybrid.defaultimplementation;

import java.util.Collection;

import org.etexascode.apps.UtilsFilterMessages;
import org.etexascode.apps.UtilsMessageImports;
import org.etexascode.apps.hybrid.interfaces.IMapParser;
import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.j2735.MapData;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Basic implementation of a mapdata parser
 * 
 * @author ablatt
 */
public class SimpleMapParser implements IMapParser {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SimpleMapParser.class);

    @Override
    public LaneManager parseLaneManager(Object[] data, LaneManager curr) {
        if (curr != null) {
            return curr;
        }

        Collection<MapData> map = UtilsFilterMessages.filterForMap(data);

        if (map.size() > 0) {
            MapData md = getAppropriateMapMessage(map);
            if (md != null) {
                return UtilsMessageImports.parseMapDataMessage(md);
            }
            else {
                return null;
            }
        }
        else {
            return null;
        }
    }

    /**
     * Currently set to grab the first MapData message.
     * 
     * @param map a collection of all the mapdata messages received this time step
     * @return the map data message to use (null if no matching mapdata message was found)
     */
    MapData getAppropriateMapMessage(Collection<MapData> map) {
        if (map.isEmpty()) {
            return null;
        }
        else {
            return map.iterator().next();
        }
    }
}
