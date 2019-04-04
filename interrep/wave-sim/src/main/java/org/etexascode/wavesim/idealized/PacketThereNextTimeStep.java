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
package org.etexascode.wavesim.idealized;

import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.wavesim.IPacketTransmission;

/**
 * Packet always make it in 1 time step
 * 
 * @author ablatt
 */
public class PacketThereNextTimeStep implements IPacketTransmission {

    /**
     * Transmits the packet in one time step
     * 
     * @param source the source of the packet
     * @param destination the destination of the packet
     * @return the packet will arrive in one time step
     */
    @Override
    public int transmitPacket(IDistanceable source, IDistanceable destination) {
        return 1;
    }
}
