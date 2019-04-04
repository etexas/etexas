/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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
package org.etexascode.j2735_2016.elements;

import java.util.Objects;

import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The lane direction element for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class LaneDirection implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover a lane direction.
     */
    public static final int NUM_BITS = 2;

    /**
     * The ingress path status.
     */
    private boolean ingressPath = false;

    /**
     * The egress path status.
     */
    private boolean egressPath = false;

    /**
     * A getter for the ingress path status.
     * 
     * @return The ingress path status.
     */
    public boolean isIngressPath() {

        return ingressPath;
    }

    /**
     * A setter for the ingress path status.
     * 
     * @param ingressPath The ingress path status to set.
     */
    public void setIngressPath(boolean ingressPath) {

        this.ingressPath = ingressPath;
    }

    /**
     * A getter for the egress path status.
     * 
     * @return The egress path status.
     */
    public boolean isEgressPath() {

        return egressPath;
    }

    /**
     * A setter for the egress path status.
     * 
     * @param egressPath The egress path status to set.
     */
    public void setEgressPath(boolean egressPath) {

        this.egressPath = egressPath;
    }

    @Override
    public String encodeUPER() {

        StringBuilder laneDirection = new StringBuilder(2);
        laneDirection.append(ingressPath ? '1' : '0');
        laneDirection.append(egressPath ? '1' : '0');

        return laneDirection.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (LaneDirection.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a LaneDirection element (%d)", LaneDirection.NUM_BITS));
        }

        String laneDirectionBits = bits.substring(0, LaneDirection.NUM_BITS);

        ingressPath = laneDirectionBits.charAt(0) == '1';
        egressPath = laneDirectionBits.charAt(1) == '1';

        return bits.substring(LaneDirection.NUM_BITS);
    }

    @Override
    public int hashCode() {

        return Objects.hash(ingressPath, egressPath);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof LaneDirection)) {

            return false;
        }
        LaneDirection element = (LaneDirection)object;
        return this.ingressPath == element.ingressPath
                && this.egressPath == element.egressPath;
    }
}
