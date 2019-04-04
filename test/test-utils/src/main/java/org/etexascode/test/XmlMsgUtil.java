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
package org.etexascode.test;

import java.io.File;
import java.util.LinkedList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.etexascode.interrep.datamodel.SignalIndication;
import org.etexascode.interrep.datamodel.SignalManager;
//import org.etexascode.interrep.datamodel.Signal;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapter;

public class XmlMsgUtil {

    public static SignalManager xmlToSignal(File xmlFile) throws JAXBException {
        JAXBContext context;
        SignalManager signalManager = null;
        context = JAXBContext.newInstance(SignalManager.class);
        final Unmarshaller unmarshaller = context.createUnmarshaller();
        unmarshaller.setAdapter(new ManagerAdapter<SignalIndication>());
        signalManager = (SignalManager)unmarshaller.unmarshal(xmlFile);

        return signalManager;

    }

    // private final Map<Integer, Signal> signals = new HashMap<Integer, Signal>();
    private final List<SignalIndication> signals = new LinkedList<SignalIndication>();
}
