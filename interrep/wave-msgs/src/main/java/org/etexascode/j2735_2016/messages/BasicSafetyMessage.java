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
package org.etexascode.j2735_2016.messages;

import java.util.Objects;

import org.etexascode.j2735_2016.frames.BSMcoreData;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The basic safety message for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class BasicSafetyMessage implements UnalignedPackedEncodingRules {

    /**
     * The number of bits that cover the optional portions of the BSM.
     */
    public static final int NUM_BITS = 3;

    /**
     * The BSM core data frame.
     */
    private BSMcoreData coreData;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public BasicSafetyMessage() {

        coreData = new BSMcoreData();
    }

    /**
     * A constructor for the basic safety message for all required fields.
     * 
     * @param coreData The the BSM core data frame.
     */
    public BasicSafetyMessage(BSMcoreData coreData) {

        this.coreData = Objects.requireNonNull(coreData);
    }

    /**
     * A getter for the BSM core data frame.
     * 
     * @return The BSM core data frame.
     */
    public BSMcoreData getCoreData() {

        return coreData;
    }

    /**
     * A setter for the BSM core data frame.
     * 
     * @param coreData The BSM core data frame to set.
     */
    public void setCoreData(BSMcoreData coreData) {

        this.coreData = Objects.requireNonNull(coreData);
    }

    @Override
    public String encodeUPER() {

        StringBuilder bsmBits = new StringBuilder();

        // we aren't planning on having the extension or regional extension usable so setting to
        // off. TODO ttevendale 1/26/2018 - implement PartIIcontent
        bsmBits.append("000");
        bsmBits.append(coreData.encodeUPER());

        return bsmBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        if (BasicSafetyMessage.NUM_BITS > bits.length()) {

            throw new IllegalArgumentException(String.format("The bits supplied were not enough for a BasicSafetyMessage frame(%d)", BasicSafetyMessage.NUM_BITS));
        }

        String bsmOptionalBits = bits.substring(0, BasicSafetyMessage.NUM_BITS);
        bits = bits.substring(BasicSafetyMessage.NUM_BITS);

        if (bsmOptionalBits.charAt(0) != '0') {

            throw new IllegalArgumentException("The BasicSafetyMessage extension is not supported");
        }

        if (bsmOptionalBits.charAt(1) != '0') {

            throw new IllegalArgumentException("The BasicSafetyMessage part II is currently not supported");
        }

        if (bsmOptionalBits.charAt(2) != '0') {

            throw new IllegalArgumentException("The BasicSafetyMessage regional extension is not supported");
        }

        bits = coreData.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hashCode(coreData);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof BasicSafetyMessage)) {

            return false;
        }
        BasicSafetyMessage message = (BasicSafetyMessage)object;
        return this.coreData.equals(message.coreData);
    }
}
