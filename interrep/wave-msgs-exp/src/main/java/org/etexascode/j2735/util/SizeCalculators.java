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
package org.etexascode.j2735.util;

import java.util.List;

import org.etexascode.j2735.exp.ApproachLaneExp;
import org.etexascode.j2735.exp.ApproachObjectExp;
import org.etexascode.j2735.exp.GIDExp;
import org.etexascode.j2735.exp.IntersectionExp;
import org.etexascode.j2735.exp.IntersectionStateExp;
import org.etexascode.j2735.exp.MovementStateExp;
import org.etexascode.j2735.exp.SPATExp;

/**
 * Utility methods to calculate the number of bytes in a WAVE message.
 * 
 * @author janway
 */
public class SizeCalculators {

    /**
     * Calculates the number of bytes in an experimental SPAT message.
     * 
     * @param msg The message to size.
     * @return The size in bytes.
     */
    public static int getSize(SPATExp msg) {
        int size = 0;
        // msgID - 1 byte, contentVersion - 1 byte, CRC OPTIONAL - 0 or 2 bytes
        // + blob size

        size += 1 + 1 + 2; // msgID, content version, size of blob
        // ----------- blob -----------
        IntersectionStateExp ise = msg.getSpatBlob();

        size += 2; // object identifier + size
        size += ise.getId().getSize(); // 4

        size += 2;
        size += ise.getStatus().getSize(); // 1

        size += 2;
        size += ise.getTimeStamp().getSize(); // 5

        List<MovementStateExp> mse = ise.getStates().getMovementState();
        for (MovementStateExp m : mse) {
            size += 1; // object identifier for movementstate (no size or payload field)

            size += 2; // object identifier (1) + size (1)
            size += m.getCurrState().getPayload().length;

            size += 2;
            size += m.getLaneSet().getPayload().length;

            size += 2;
            size += m.getMaxTimeRemaining().getPayload().length; // 2

            size += 2;
            size += m.getMinTimeRemaining().getPayload().length; // 2

            // size += 2;
            // size += m.getPedCount().getPayload().length; //optional
            //
            // size += 2;
            // size += m.getPedDetect().getPayload().length; //optional
            //
            // size += 2;
            // size += m.getYellState().getPayload().length; //optional
            //
            // size += 2;
            // size += m.getYellTimeToChange().getPayload().length; //required with YellState
        }

        size += 1; // end of blob
        // ---------- end blob -----------

        // size += 2; // crc optional

        return size;
    }

    /**
     * Calculates the number of bytes in an experimental GID message.
     * 
     * @param msg The message to size.
     * @return The size in bytes.
     */
    public static int getSize(GIDExp msg) {
        int size = 0;

        size += 1 + 1 + 2; // msgID, content version, size of blob

        // ------ blob -----------
        List<IntersectionExp> lie = msg.getIntersections().getIntersection();
        for (IntersectionExp ie : lie) {
            size += 2;
            size += ie.getAttributes().getSize(); // 1

            size += 2;
            size += ie.getIntersectionId().getSize(); // 4

            size += 2;
            size += ie.getReferencePoint().getSize(); // 8

            List<ApproachObjectExp> approaches = ie.getApproaches().getApproachObject();
            for (ApproachObjectExp aoe : approaches) {
                size += 2;
                size += aoe.getApproach().getSize(); // 1

                List<ApproachLaneExp> lale = aoe.getVehicleLanes().getApproachLaneObject();
                for (ApproachLaneExp ale : lale) {
                    size += 2;
                    size += ale.getLane().getSize(); // 2

                    size += 2;
                    size += ale.getLaneAttributes().getSize(); // 2

                    size += 2;
                    size += ale.getNodeList().getSize();
                }
            }
        }

        size += 1; // end of blob

        // ------ end of blob ---------

        // size += 2; // crc optional

        return size;
    }

}
