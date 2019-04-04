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

import org.etexascode.j2735_2016.elements.SemiMajorAxisAccuracy;
import org.etexascode.j2735_2016.elements.SemiMajorAxisOrientation;
import org.etexascode.j2735_2016.elements.SemiMinorAxisAccuracy;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The positional accuracy frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class PositionalAccuracy implements UnalignedPackedEncodingRules {

    /**
     * The semi major axis accuracy element.
     */
    private SemiMajorAxisAccuracy semiMajor;

    /**
     * The semi minor axis accuracy element.
     */
    private SemiMinorAxisAccuracy semiMinor;

    /**
     * The semi major axis orientation element.
     */
    private SemiMajorAxisOrientation orientation;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public PositionalAccuracy() {

        this.semiMajor = new SemiMajorAxisAccuracy();
        this.semiMinor = new SemiMinorAxisAccuracy();
        this.orientation = new SemiMajorAxisOrientation();
    }

    /**
     * A constructor for the positional accuracy.
     * 
     * @param semiMajor The semi major axis accuracy element.
     * @param semiMinor The semi minor axis accuracy element.
     * @param orientation The semi major axis orientation element.
     */
    public PositionalAccuracy(SemiMajorAxisAccuracy semiMajor, SemiMinorAxisAccuracy semiMinor, SemiMajorAxisOrientation orientation) {

        this.semiMajor = Objects.requireNonNull(semiMajor);
        this.semiMinor = Objects.requireNonNull(semiMinor);
        this.orientation = Objects.requireNonNull(orientation);
    }

    /**
     * A constructor for the positional accuracy which allows primitive/enumerated data to be passed
     * to all primitive/enumerated elements.
     * 
     * @param semiMajor The semi major axis accuracy value.
     * @param semiMinor The semi minor axis accuracy value.
     * @param orientation The semi major axis orientation value.
     */
    public PositionalAccuracy(int semiMajor, int semiMinor, int orientation) {

        this.semiMajor = new SemiMajorAxisAccuracy(semiMajor);
        this.semiMinor = new SemiMinorAxisAccuracy(semiMinor);
        this.orientation = new SemiMajorAxisOrientation(orientation);
    }

    /**
     * A getter for the semi major axis accuracy element.
     * 
     * @return The semi major axis accuracy element.
     */
    public SemiMajorAxisAccuracy getSemiMajor() {

        return semiMajor;
    }

    /**
     * A setter for the semi major axis accuracy element.
     * 
     * @param semiMajor The semi major axis accuracy element to set.
     */
    public void setSemiMajor(SemiMajorAxisAccuracy semiMajor) {

        this.semiMajor = Objects.requireNonNull(semiMajor);
    }

    /**
     * A setter for the semi major axis accuracy element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param semiMajor The semi major axis accuracy value to be set in the element.
     */
    public void setSemiMajor(int semiMajor) {

        this.semiMajor.setValue(semiMajor);
    }

    /**
     * A getter for the The semi minor axis accuracy element.
     * 
     * @return The The semi minor axis accuracy element.
     */
    public SemiMinorAxisAccuracy getSemiMinor() {

        return semiMinor;
    }

    /**
     * A setter for the The semi minor axis accuracy element.
     * 
     * @param semiMinor The The semi minor axis accuracy element to set.
     */
    public void setSemiMinor(SemiMinorAxisAccuracy semiMinor) {

        this.semiMinor = Objects.requireNonNull(semiMinor);
    }

    /**
     * A setter for the The semi minor axis accuracy element. Allows primitive data to be passed to
     * the primitive element.
     * 
     * @param semiMinor The The semi minor axis accuracy value to be set in the element.
     */
    public void setSemiMinor(int semiMinor) {

        this.semiMinor.setValue(semiMinor);
    }

    /**
     * A getter for the semi major axis orientation element.
     * 
     * @return The semi major axis orientation element.
     */
    public SemiMajorAxisOrientation getOrientation() {

        return orientation;
    }

    /**
     * A setter for the semi major axis orientation element.
     * 
     * @param orientation The semi major axis orientation element to set.
     */
    public void setOrientation(SemiMajorAxisOrientation orientation) {

        this.orientation = Objects.requireNonNull(orientation);
    }

    /**
     * A setter for the semi major axis orientation element. Allows primitive data to be passed to
     * the primitive element.
     * 
     * @param orientation The semi major axis orientation value to be set in the element.
     */
    public void setOrientation(int orientation) {

        this.orientation.setValue(orientation);
    }

    @Override
    public String encodeUPER() {

        StringBuilder positionalAccuracy = new StringBuilder();

        positionalAccuracy.append(semiMajor.encodeUPER());
        positionalAccuracy.append(semiMinor.encodeUPER());
        positionalAccuracy.append(orientation.encodeUPER());

        return positionalAccuracy.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = semiMajor.decodeUPER(bits);
        bits = semiMinor.decodeUPER(bits);
        bits = orientation.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(semiMajor, semiMinor, orientation);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof PositionalAccuracy)) {

            return false;
        }
        PositionalAccuracy frame = (PositionalAccuracy)object;
        return this.semiMajor.equals(frame.semiMajor)
                && this.semiMinor.equals(frame.semiMinor)
                && this.orientation.equals(frame.orientation);
    }
}
