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

import org.etexascode.j2735_2016.elements.Acceleration;
import org.etexascode.j2735_2016.elements.VerticalAcceleration;
import org.etexascode.j2735_2016.elements.YawRate;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The acceleration set four way frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class AccelerationSet4Way implements UnalignedPackedEncodingRules {

    /**
     * The longitude acceleration element.
     */
    private Acceleration longitude;

    /**
     * The latitude acceleration element.
     */
    private Acceleration latitude;

    /**
     * The vertical acceleration element.
     */
    private VerticalAcceleration vertical;

    /**
     * The yaw rate element.
     */
    private YawRate yawRate;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public AccelerationSet4Way() {

        this.longitude = new Acceleration();
        this.latitude = new Acceleration();
        this.vertical = new VerticalAcceleration();
        this.yawRate = new YawRate();
    }

    /**
     * A constructor for the acceleration set four way.
     * 
     * @param longitude The longitude acceleration element.
     * @param latitude The latitude acceleration element.
     * @param vertical The vertical acceleration element.
     * @param yawRate The yaw rate element.
     */
    public AccelerationSet4Way(Acceleration longitude, Acceleration latitude, VerticalAcceleration vertical, YawRate yawRate) {

        this.longitude = Objects.requireNonNull(longitude);
        this.latitude = Objects.requireNonNull(latitude);
        this.vertical = Objects.requireNonNull(vertical);
        this.yawRate = Objects.requireNonNull(yawRate);
    }

    /**
     * A constructor for the acceleration set four way which allows primitive/enumerated data to be
     * passed to all primitive/enumerated elements.
     * 
     * @param longitude The longitude acceleration value.
     * @param latitude The latitude acceleration value.
     * @param vertical The vertical acceleration value.
     * @param yawRate The yaw rate value.
     */
    public AccelerationSet4Way(int longitude, int latitude, int vertical, int yawRate) {

        this.longitude = new Acceleration(longitude);
        this.latitude = new Acceleration(latitude);
        this.vertical = new VerticalAcceleration(vertical);
        this.yawRate = new YawRate(yawRate);
    }

    /**
     * A getter for the longitude acceleration element.
     * 
     * @return The longitude acceleration element.
     */
    public Acceleration getLongitude() {

        return longitude;
    }

    /**
     * A setter for the longitude acceleration element.
     * 
     * @param longitude The longitude acceleration element to set.
     */
    public void setLongitude(Acceleration longitude) {

        this.longitude = Objects.requireNonNull(longitude);
    }

    /**
     * A setter for the longitude acceleration element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param longitude The longitude acceleration value to be set in the element.
     */
    public void setLongitude(int longitude) {

        this.longitude.setValue(longitude);
    }

    /**
     * A getter for the latitude acceleration element.
     * 
     * @return The latitude acceleration element.
     */
    public Acceleration getLatitude() {

        return latitude;
    }

    /**
     * A setter for the latitude acceleration element.
     * 
     * @param latitude The latitude acceleration element to set.
     */
    public void setLatitude(Acceleration latitude) {

        this.latitude = Objects.requireNonNull(latitude);
    }

    /**
     * A setter for the latitude acceleration element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param latitude The latitude acceleration value to be set in the element.
     */
    public void setLatitude(int latitude) {

        this.latitude.setValue(latitude);
    }

    /**
     * A getter for the vertical acceleration element.
     * 
     * @return The vertical acceleration element.
     */
    public VerticalAcceleration getVertical() {

        return vertical;
    }

    /**
     * A setter for the vertical acceleration element.
     * 
     * @param vertical The vertical acceleration element to set.
     */
    public void setVertical(VerticalAcceleration vertical) {

        this.vertical = Objects.requireNonNull(vertical);
    }

    /**
     * A setter for the vertical acceleration element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param vertical The vertical acceleration value to be set in the element.
     */
    public void setVertical(int vertical) {

        this.vertical.setValue(vertical);
    }

    /**
     * A getter for the yaw rate element.
     * 
     * @return The yaw rate element.
     */
    public YawRate getYawRate() {

        return yawRate;
    }

    /**
     * A setter for the yaw rate element.
     * 
     * @param yawRate The yaw rate element to set.
     */
    public void setYawRate(YawRate yawRate) {

        this.yawRate = Objects.requireNonNull(yawRate);
    }

    /**
     * A setter for the yaw rate element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param yawRate The yaw rate value to be set in the element.
     */
    public void setYawRate(int yawRate) {

        this.yawRate.setValue(yawRate);
    }

    @Override
    public String encodeUPER() {

        StringBuilder accelerationSet4WayBits = new StringBuilder();

        accelerationSet4WayBits.append(longitude.encodeUPER());
        accelerationSet4WayBits.append(latitude.encodeUPER());
        accelerationSet4WayBits.append(vertical.encodeUPER());
        accelerationSet4WayBits.append(yawRate.encodeUPER());

        return accelerationSet4WayBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = longitude.decodeUPER(bits);
        bits = latitude.decodeUPER(bits);
        bits = vertical.decodeUPER(bits);
        bits = yawRate.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(longitude, latitude, vertical, yawRate);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof AccelerationSet4Way)) {

            return false;
        }
        AccelerationSet4Way frame = (AccelerationSet4Way)object;
        return this.longitude.equals(frame.longitude)
                && this.latitude.equals(frame.latitude)
                && this.vertical.equals(frame.vertical)
                && this.yawRate.equals(frame.yawRate);
    }
}
