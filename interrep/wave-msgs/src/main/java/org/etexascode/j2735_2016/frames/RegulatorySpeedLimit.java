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
package org.etexascode.j2735_2016.frames;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.SpeedLimitType;
import org.etexascode.j2735_2016.elements.SpeedLimitType.SpeedLimit;
import org.etexascode.j2735_2016.elements.Velocity;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The regulatory speed limit frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RegulatorySpeedLimit implements UnalignedPackedEncodingRules {

    /**
     * The speed limit type element.
     */
    private SpeedLimitType type;

    /**
     * The velocity element.
     */
    private Velocity speed;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public RegulatorySpeedLimit() {

        this.type = new SpeedLimitType();
        this.speed = new Velocity();
    }

    /**
     * A constructor for the regulatory speed limit.
     * 
     * @param type The speed limit type element.
     * @param speed The velocity element.
     */
    public RegulatorySpeedLimit(SpeedLimitType type, Velocity speed) {

        this.type = Objects.requireNonNull(type);
        this.speed = Objects.requireNonNull(speed);
    }

    /**
     * A constructor for the regulatory speed limit way which allows primitive/enumerated data to be
     * passed to all primitive/enumerated elements.
     * 
     * @param type The speed limit type enumeration.
     * @param speed The velocity value.
     */
    public RegulatorySpeedLimit(SpeedLimit type, int speed) {

        this.type = new SpeedLimitType(Objects.requireNonNull(type));
        this.speed = new Velocity(speed);
    }

    /**
     * A getter for the speed limit type element.
     * 
     * @return The speed limit type element.
     */
    public SpeedLimitType getType() {

        return type;
    }

    /**
     * A setter for the speed limit type element.
     * 
     * @param type The speed limit type element to set.
     */
    public void setType(SpeedLimitType type) {

        this.type = Objects.requireNonNull(type);
    }

    /**
     * A setter for the speed limit type element. Allows enumeration data to be passed to the
     * enumeration element.
     * 
     * @param type The speed limit enumeration to be set in the element.
     */
    public void setType(SpeedLimit type) {

        this.type.setEnumeration(Objects.requireNonNull(type));
    }

    /**
     * A getter for the velocity element.
     * 
     * @return The velocity element.
     */
    public Velocity getSpeed() {

        return speed;
    }

    /**
     * A setter for the velocity element.
     * 
     * @param speed The velocity element to set.
     */
    public void setSpeed(Velocity speed) {

        this.speed = Objects.requireNonNull(speed);
    }

    /**
     * A setter for the velocity element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param speed The speed value to be set in the element.
     */
    public void setSpeed(int speed) {

        this.speed.setValue(speed);
    }

    @Override
    public String encodeUPER() {

        StringBuilder regulatorySpeedLimitBits = new StringBuilder();

        regulatorySpeedLimitBits.append(type.encodeUPER());
        regulatorySpeedLimitBits.append(speed.encodeUPER());

        return regulatorySpeedLimitBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = type.decodeUPER(bits);
        bits = speed.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(type, speed);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RegulatorySpeedLimit)) {

            return false;
        }
        RegulatorySpeedLimit frame = (RegulatorySpeedLimit)object;
        return this.type.equals(frame.type)
                && this.speed.equals(frame.speed);
    }
}
