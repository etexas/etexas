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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * @author bbadillo
 */
public class SocketReceiver {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws MalformedURLException, IOException {
        // NativeLibrary.getInstance("nanomsg", new URLClassLoader(new URL[]{new
        // File(".").toURL()}));

        if (args.length != 1) {
            System.err.println("Usage: command <port number>");
            System.exit(1);
        }

        int portNumber = Integer.parseInt(args[0]);

        ServerSocket server = new ServerSocket(portNumber);

        Socket s = server.accept();

        OutputStreamWriter osw = new OutputStreamWriter(s.getOutputStream(), "UTF-8");
        PrintWriter out = new PrintWriter(osw, true);
        BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream()));

        // Block until the client is started to kick off the benchmark.
        String startData = in.readLine();
        out.println(startData);

        // Start benchmarking
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < 100000; i++) {
            String recievedData = in.readLine();
            out.println(recievedData);
            System.out.println(i);
        }
        long endTime = System.currentTimeMillis();

        s.close();
        server.close();

        System.out.println("Running time: " + (endTime - startTime));
    }

}
