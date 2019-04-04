/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2017 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
-
SBIR DATA RIGHTS
Harmonia Holdings Group, LLC
2020 Kraft Drive Suite 2400
Blacksburg, VA 24060
Contract No: DTRT57-16-c-10008
Start Date: 01/05/2016
End Date: 01/05/2018
Expiration of SBIR Data Rights Period: 01/05/2022
-
The Government's rights to use, modify, reproduce, release, perform,
display, or disclose technical data or computer software marked with
this legend are restricted during the period shown as provided in
paragraph (b)(4) of the Rights in Noncommercial Technical Data and
Computer Software-Small Business Innovation Research (SBIR) Program
clause contained in the above identified contract. No restrictions
apply after the expiration date shown above. Any reproduction of
technical data, computer software, or portions thereof marked with
this legend must also reproduce the markings.
-
Contributors:
Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */
package org.etexascode.datalayer.inmemory;

import java.awt.Polygon;
import java.util.Collection;

import org.etexascode.datalayer.interfaces.IMessageComponent;
import org.etexascode.datalayer.interfaces.ITemporalComponent;
import org.etexascode.datalayer.interfaces.IWaveSimDataLayer;
import org.etexascode.wavesim.Rx;
import org.etexascode.wavesim.Tx;

/**
 * The default WAVE simulation data layer.
 * 
 * @author ablatt
 * @author ttevendale
 * @author emyers
 */
public class DefaultWaveSimDataLayer implements IWaveSimDataLayer {

    /** The message component. */
    private IMessageComponent messageComponent;

    /** The temporal component. */
    private ITemporalComponent temporalComponent;

    /**
     * Creates a new <code>DefaultWaveSimDataLayer</code> with the specified components.
     * 
     * @param messageComponent The message component for this data layer.
     * @param temporalComponent The temporal component for this data layer.
     */
    public DefaultWaveSimDataLayer(IMessageComponent messageComponent, ITemporalComponent temporalComponent) {

        this.messageComponent = messageComponent;
        this.temporalComponent = temporalComponent;
    }

    @Override
    public Iterable<Tx> getTxs(int stepNum, Polygon segment) {

        return messageComponent.getWaveSimInputs(stepNum, segment);
    }

    @Override
    public void putMessages(int stepNum, Collection<Rx> nodes) {

        messageComponent.putWaveSimOutputs(stepNum, nodes);
    }

    @Override
    public double getSimTime(int stepNum) {

        return temporalComponent.getSimTime(stepNum);
    }

    @Override
    public double getStepSize() {

        return temporalComponent.getStepSize();
    }
}
