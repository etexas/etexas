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

import java.net.MalformedURLException;
import java.util.logging.Level;
import java.util.logging.Logger;
import nanomsg.reqrep.RepSocket;

/**
 * @author bbadillo
 */
public class NanoReceiver {

    /**
     * @param args the command line arguments
     * @throws java.net.MalformedURLException
     */
    public static void main(String[] args) throws MalformedURLException {
        if (args.length != 1) {
            System.err.println(
                    "Usage: command <port number>");
            System.exit(1);
        }

        int portNumber = Integer.parseInt(args[0]);
        System.out.println("Started...");

        RepSocket s = new RepSocket();
        s.setRecvTimeout(60000); // 1 minute wait

        try {
            s.bind("tcp://*:" + portNumber);
        }
        catch (nanomsg.exceptions.IOException ex) {
            Logger.getLogger(NanoReceiver.class.getName()).log(Level.SEVERE, null, ex);
        }

        // Block until the client is started to kick off the benchmark.
        byte[] startData;
        try {
            startData = s.recvBytes();
            s.sendBytes(startData);
        }
        catch (nanomsg.exceptions.IOException ex) {
            Logger.getLogger(NanoReceiver.class.getName()).log(Level.SEVERE, null, ex);
        }

        // Start benchmarking
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < 100000; i++) {
            try {
                byte[] receivedData = s.recvBytes();
                s.sendBytes(receivedData);
            }
            catch (nanomsg.exceptions.IOException ex) {
                Logger.getLogger(NanoReceiver.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        long endTime = System.currentTimeMillis();

        System.out.println("Running time: " + (endTime - startTime));
    }

}
