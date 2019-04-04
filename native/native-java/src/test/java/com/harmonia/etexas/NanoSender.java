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

package com.harmonia.etexas;

import java.util.logging.Level;
import java.util.logging.Logger;
import nanomsg.reqrep.ReqSocket;

/**
 * Sends some test data to the native-agent running at the host name and port number given on the
 * command line.
 *
 * @author janway
 */
public class NanoSender {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: command <host name> <port number>");
            System.exit(1);
        }

        String hostName = args[0];
        int portNumber = Integer.parseInt(args[1]);

        ReqSocket s = new ReqSocket();
        try {
            s.connect("tcp://" + hostName + ":" + portNumber);

            // Send message that client is ready to start benchmarking
            s.sendString("Start!");
            s.recvString();

            // Start benchmarking
            for (int i = 0; i < 100000; i++) {
                s.sendString("Hello!");
                s.recvString();
            }
            s.close();
        }
        catch (nanomsg.exceptions.IOException ex) {
            Logger.getLogger(NanoSender.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
