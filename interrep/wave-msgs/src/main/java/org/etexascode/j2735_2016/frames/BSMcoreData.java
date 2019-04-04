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

import org.etexascode.j2735_2016.elements.DSecond;
import org.etexascode.j2735_2016.elements.Elevation;
import org.etexascode.j2735_2016.elements.Heading;
import org.etexascode.j2735_2016.elements.Latitude;
import org.etexascode.j2735_2016.elements.Longitude;
import org.etexascode.j2735_2016.elements.MsgCount;
import org.etexascode.j2735_2016.elements.Speed;
import org.etexascode.j2735_2016.elements.SteeringWheelAngle;
import org.etexascode.j2735_2016.elements.TemporaryID;
import org.etexascode.j2735_2016.elements.TransmissionState;
import org.etexascode.j2735_2016.elements.TransmissionState.Transmission;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The BSM core data frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class BSMcoreData implements UnalignedPackedEncodingRules {

    /**
     * The message count element.
     */
    private MsgCount messageCount = new MsgCount();

    /**
     * The temporary ID element.
     */
    private TemporaryID id = new TemporaryID();

    /**
     * The second element.
     */
    private DSecond secMark = new DSecond();

    /**
     * The latitude element.
     */
    private Latitude latitude = new Latitude();

    /**
     * The longitude element.
     */
    private Longitude longitude = new Longitude();

    /**
     * The elevation element.
     */
    private Elevation elevation = new Elevation();

    /**
     * The positional accuracy frame.
     */
    private PositionalAccuracy accuracy = new PositionalAccuracy();

    /**
     * The transmission state element.
     */
    private TransmissionState transmission = new TransmissionState();

    /**
     * The speed element.
     */
    private Speed speed = new Speed();

    /**
     * The heading element.
     */
    private Heading heading = new Heading();

    /**
     * The steering wheel angle element.
     */
    private SteeringWheelAngle angle = new SteeringWheelAngle();

    /**
     * The acceleration set frame.
     */
    private AccelerationSet4Way accelerationSet = new AccelerationSet4Way();

    /**
     * The brake system status frame.
     */
    private BrakeSystemStatus brakes = new BrakeSystemStatus();

    /**
     * The vehicle size frame.
     */
    private VehicleSize size = new VehicleSize();

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public BSMcoreData() {

        messageCount = new MsgCount();
        id = new TemporaryID();
        secMark = new DSecond();
        latitude = new Latitude();
        longitude = new Longitude();
        elevation = new Elevation();
        accuracy = new PositionalAccuracy();
        transmission = new TransmissionState();
        speed = new Speed();
        heading = new Heading();
        angle = new SteeringWheelAngle();
        accelerationSet = new AccelerationSet4Way();
        brakes = new BrakeSystemStatus();
        size = new VehicleSize();
    }

    /**
     * A constructor for the BSM core data.
     * 
     * @param messageCount The message count element.
     * @param id The temporary ID element.
     * @param secMark The second element.
     * @param latitude The latitude element.
     * @param longitude The longitude element.
     * @param elevation The elevation element.
     * @param accuracy The positional accuracy frame.
     * @param transmission The transmission state element.
     * @param speed The speed element.
     * @param heading The heading element.
     * @param angle The steering wheel angle element.
     * @param accelerationSet The acceleration set frame.
     * @param brakes The brake system status frame.
     * @param size The vehicle size frame.
     */
    public BSMcoreData(MsgCount messageCount, TemporaryID id, DSecond secMark, Latitude latitude, Longitude longitude, Elevation elevation, PositionalAccuracy accuracy, TransmissionState transmission,
            Speed speed, Heading heading, SteeringWheelAngle angle, AccelerationSet4Way accelerationSet, BrakeSystemStatus brakes, VehicleSize size) {

        this.messageCount = Objects.requireNonNull(messageCount);
        this.id = Objects.requireNonNull(id);
        this.secMark = Objects.requireNonNull(secMark);
        this.latitude = Objects.requireNonNull(latitude);
        this.longitude = Objects.requireNonNull(longitude);
        this.elevation = Objects.requireNonNull(elevation);
        this.accuracy = Objects.requireNonNull(accuracy);
        this.transmission = Objects.requireNonNull(transmission);
        this.speed = Objects.requireNonNull(speed);
        this.heading = Objects.requireNonNull(heading);
        this.angle = Objects.requireNonNull(angle);
        this.accelerationSet = Objects.requireNonNull(accelerationSet);
        this.brakes = Objects.requireNonNull(brakes);
        this.size = Objects.requireNonNull(size);
    }

    /**
     * A constructor for the BSM core data which allows primitive/enumerated data to be passed to
     * all primitive/enumerated elements.
     * 
     * @param messageCount The message count value.
     * @param id The temporary ID value.
     * @param secMark The second value.
     * @param latitude The latitude value.
     * @param longitude The longitude value.
     * @param elevation The elevation value.
     * @param accuracy The positional accuracy frame.
     * @param transmission The transmission state enumeration.
     * @param speed The speed value.
     * @param heading The heading value.
     * @param angle The steering wheel angle value.
     * @param accelerationSet The acceleration set frame.
     * @param brakes The brake system status frame.
     * @param size The vehicle size frame.
     */
    public BSMcoreData(int messageCount, String id, int secMark, int latitude, long longitude, int elevation, PositionalAccuracy accuracy, Transmission transmission, int speed, int heading, int angle,
            AccelerationSet4Way accelerationSet, BrakeSystemStatus brakes, VehicleSize size) {

        this.messageCount = new MsgCount(messageCount);
        this.id = new TemporaryID(Objects.requireNonNull(id));
        this.secMark = new DSecond(secMark);
        this.latitude = new Latitude(latitude);
        this.longitude = new Longitude(longitude);
        this.elevation = new Elevation(elevation);
        this.accuracy = Objects.requireNonNull(accuracy);
        this.transmission = new TransmissionState(Objects.requireNonNull(transmission));
        this.speed = new Speed(speed);
        this.heading = new Heading(heading);
        this.angle = new SteeringWheelAngle(angle);
        this.accelerationSet = Objects.requireNonNull(accelerationSet);
        this.brakes = Objects.requireNonNull(brakes);
        this.size = Objects.requireNonNull(size);
    }

    /**
     * A getter for the message count element.
     * 
     * @return The message count element.
     */
    public MsgCount getMessageCount() {

        return messageCount;
    }

    /**
     * A setter for the message count element.
     * 
     * @param messageCount The message count element to set.
     */
    public void setMessageCount(MsgCount messageCount) {

        this.messageCount = Objects.requireNonNull(messageCount);
    }

    /**
     * A setter for the message count element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param messageCount The message count value to be set in the element.
     */
    public void setMessageCount(int messageCount) {

        this.messageCount.setValue(messageCount);
    }

    /**
     * A getter for the temporary ID element.
     * 
     * @return The temporary ID element.
     */
    public TemporaryID getId() {

        return id;
    }

    /**
     * A setter for the temporary ID element.
     * 
     * @param id The temporary ID element to set.
     */
    public void setId(TemporaryID id) {

        this.id = Objects.requireNonNull(id);
    }

    /**
     * A setter for the temporary ID element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param id The temporary ID value to be set in the element.
     */
    public void setId(String id) {

        this.id.setValue(Objects.requireNonNull(id));
    }

    /**
     * A getter for the second element.
     * 
     * @return The second element.
     */
    public DSecond getSecMark() {

        return secMark;
    }

    /**
     * A setter for the second element.
     * 
     * @param secMark The second element to set.
     */
    public void setSecMark(DSecond secMark) {

        this.secMark = Objects.requireNonNull(secMark);
    }

    /**
     * A setter for the second element. Allows primitive data to be passed to the primitive element.
     * 
     * @param secMark The second value to be set in the element.
     */
    public void setSecMark(int secMark) {

        this.secMark.setValue(secMark);
    }

    /**
     * A getter for the latitude element.
     * 
     * @return The latitude element.
     */
    public Latitude getLatitude() {

        return latitude;
    }

    /**
     * A setter for the latitude element.
     * 
     * @param latitude The latitude element to set.
     */
    public void setLatitude(Latitude latitude) {

        this.latitude = Objects.requireNonNull(latitude);
    }

    /**
     * A setter for the latitude element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param latitude The latitude value to be set in the element.
     */
    public void setLatitude(int latitude) {

        this.latitude.setValue(latitude);
    }

    /**
     * A getter for the longitude element.
     * 
     * @return The longitude element.
     */
    public Longitude getLongitude() {

        return longitude;
    }

    /**
     * A setter for the longitude element.
     * 
     * @param longitude The longitude element to set.
     */
    public void setLongitude(Longitude longitude) {

        this.longitude = Objects.requireNonNull(longitude);
    }

    /**
     * A setter for the longitude element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param longitude The longitude value to be set in the element.
     */
    public void setLongitude(long longitude) {

        this.longitude.setValue(longitude);
    }

    /**
     * A getter for the elevation element.
     * 
     * @return The elevation element.
     */
    public Elevation getElevation() {

        return elevation;
    }

    /**
     * A setter for the elevation element.
     * 
     * @param elevation The elevation element to set.
     */
    public void setElevation(Elevation elevation) {

        this.elevation = Objects.requireNonNull(elevation);
    }

    /**
     * A setter for the elevation element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param elevation The elevation value to be set in the element.
     */
    public void setElevation(int elevation) {

        this.elevation.setValue(elevation);
    }

    /**
     * A getter for the positional accuracy frame.
     * 
     * @return The positional accuracy frame.
     */
    public PositionalAccuracy getAccuracy() {

        return accuracy;
    }

    /**
     * A setter for the positional accuracy frame.
     * 
     * @param accuracy The positional accuracy frame to set.
     */
    public void setAccuracy(PositionalAccuracy accuracy) {

        this.accuracy = Objects.requireNonNull(accuracy);
    }

    /**
     * A getter for the transmission state element.
     * 
     * @return The transmission state element.
     */
    public TransmissionState getTransmission() {

        return transmission;
    }

    /**
     * A setter for the transmission state element.
     * 
     * @param transmission The transmission state element.
     */
    public void setTransmission(TransmissionState transmission) {

        this.transmission = Objects.requireNonNull(transmission);
    }

    /**
     * A setter for the transmission state element. Allows enumerated data to be passed to the
     * enumerated element.
     * 
     * @param transmission The transmission enumeration to be set in the element.
     */
    public void setTransmission(Transmission transmission) {

        this.transmission.setEnumeration(Objects.requireNonNull(transmission));
    }

    /**
     * A getter for the speed element.
     * 
     * @return The speed element.
     */
    public Speed getSpeed() {

        return speed;
    }

    /**
     * A setter for the speed element.
     * 
     * @param speed The speed element to set.
     */
    public void setSpeed(Speed speed) {

        this.speed = Objects.requireNonNull(speed);
    }

    /**
     * A setter for the speed element. Allows primitive data to be passed to the primitive element.
     * 
     * @param speed The speed value to be set in the element.
     */
    public void setSpeed(int speed) {

        this.speed.setValue(speed);
    }

    /**
     * A getter for the heading element.
     * 
     * @return The heading element.
     */
    public Heading getHeading() {

        return heading;
    }

    /**
     * A setter for the heading element.
     * 
     * @param heading The heading element to set.
     */
    public void setHeading(Heading heading) {

        this.heading = Objects.requireNonNull(heading);
    }

    /**
     * A setter for the heading element. Allows primitive data to be passed to the primitive
     * element.
     * 
     * @param heading The heading value to be set in the element.
     */
    public void setHeading(int heading) {

        this.heading.setValue(heading);
    }

    /**
     * A getter for the steering wheel angle element.
     * 
     * @return The steering wheel angle element.
     */
    public SteeringWheelAngle getAngle() {

        return angle;
    }

    /**
     * A setter for the steering wheel angle element.
     * 
     * @param angle The steering wheel angle element.
     */
    public void setAngle(SteeringWheelAngle angle) {

        this.angle = Objects.requireNonNull(angle);
    }

    /**
     * A setter for the steering wheel angle element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param angle The steering wheel angle value to be set in the element.
     */
    public void setAngle(int angle) {

        this.angle.setValue(angle);
    }

    /**
     * A getter for the acceleration set frame.
     * 
     * @return The acceleration set frame.
     */
    public AccelerationSet4Way getAccelerationSet() {

        return accelerationSet;
    }

    /**
     * A setter for the acceleration set frame.
     * 
     * @param accelerationSet The acceleration set frame to set.
     */
    public void setAccelerationSet(AccelerationSet4Way accelerationSet) {

        this.accelerationSet = Objects.requireNonNull(accelerationSet);
    }

    /**
     * A getter for the brake system status frame.
     * 
     * @return The brake system status frame.
     */
    public BrakeSystemStatus getBrakes() {

        return brakes;
    }

    /**
     * A setter for the brake system status frame.
     * 
     * @param brakes The brake system status frame to set.
     */
    public void setBrakes(BrakeSystemStatus brakes) {

        this.brakes = Objects.requireNonNull(brakes);
    }

    /**
     * A getter for the vehicle size frame.
     * 
     * @return The vehicle size frame.
     */
    public VehicleSize getSize() {

        return size;
    }

    /**
     * A setter for the vehicle size frame.
     * 
     * @param size The vehicle size frame to set.
     */
    public void setSize(VehicleSize size) {

        this.size = Objects.requireNonNull(size);
    }

    @Override
    public String encodeUPER() {

        StringBuilder bsmCoreData = new StringBuilder();

        bsmCoreData.append(messageCount.encodeUPER());
        bsmCoreData.append(id.encodeUPER());
        bsmCoreData.append(secMark.encodeUPER());
        bsmCoreData.append(latitude.encodeUPER());
        bsmCoreData.append(longitude.encodeUPER());
        bsmCoreData.append(elevation.encodeUPER());
        bsmCoreData.append(accuracy.encodeUPER());
        bsmCoreData.append(transmission.encodeUPER());
        bsmCoreData.append(speed.encodeUPER());
        bsmCoreData.append(heading.encodeUPER());
        bsmCoreData.append(angle.encodeUPER());
        bsmCoreData.append(accelerationSet.encodeUPER());
        bsmCoreData.append(brakes.encodeUPER());
        bsmCoreData.append(size.encodeUPER());

        return bsmCoreData.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = messageCount.decodeUPER(bits);
        bits = id.decodeUPER(bits);
        bits = secMark.decodeUPER(bits);
        bits = latitude.decodeUPER(bits);
        bits = longitude.decodeUPER(bits);
        bits = elevation.decodeUPER(bits);
        bits = accuracy.decodeUPER(bits);
        bits = transmission.decodeUPER(bits);
        bits = speed.decodeUPER(bits);
        bits = heading.decodeUPER(bits);
        bits = angle.decodeUPER(bits);
        bits = accelerationSet.decodeUPER(bits);
        bits = brakes.decodeUPER(bits);
        bits = size.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(messageCount, id, secMark, latitude, longitude, elevation, accuracy, transmission, speed, heading, angle, accelerationSet, brakes, size);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof BSMcoreData)) {

            return false;
        }
        BSMcoreData frame = (BSMcoreData)object;
        return this.messageCount.equals(frame.messageCount)
                && this.id.equals(frame.id)
                && this.secMark.equals(frame.secMark)
                && this.latitude.equals(frame.latitude)
                && this.longitude.equals(frame.longitude)
                && this.elevation.equals(frame.elevation)
                && this.accuracy.equals(frame.accuracy)
                && this.transmission.equals(frame.transmission)
                && this.speed.equals(frame.speed)
                && this.heading.equals(frame.heading)
                && this.angle.equals(frame.angle)
                && this.accelerationSet.equals(frame.accelerationSet)
                && this.brakes.equals(frame.brakes)
                && this.size.equals(frame.size);
    }
}
