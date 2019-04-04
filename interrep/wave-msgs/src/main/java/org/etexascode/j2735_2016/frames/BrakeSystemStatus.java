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

import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus;
import org.etexascode.j2735_2016.elements.AntiLockBrakeStatus.AntiLockBrake;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus;
import org.etexascode.j2735_2016.elements.AuxiliaryBrakeStatus.AuxiliaryBrake;
import org.etexascode.j2735_2016.elements.BrakeAppliedStatus;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied;
import org.etexascode.j2735_2016.elements.BrakeBoostApplied.BrakeBoost;
import org.etexascode.j2735_2016.elements.StabilityControlStatus;
import org.etexascode.j2735_2016.elements.StabilityControlStatus.StabilityControl;
import org.etexascode.j2735_2016.elements.TractionControlStatus;
import org.etexascode.j2735_2016.elements.TractionControlStatus.TractionControl;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The brake system status frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class BrakeSystemStatus implements UnalignedPackedEncodingRules {

    /**
     * The brake applied status element.
     */
    private BrakeAppliedStatus wheelBrakes;

    /**
     * The traction control status element.
     */
    private TractionControlStatus traction;

    /**
     * The anti lock brake status element.
     */
    private AntiLockBrakeStatus abs;

    /**
     * The stability control status element.
     */
    private StabilityControlStatus scs;

    /**
     * The brake boost applied status element.
     */
    private BrakeBoostApplied brakeBoost;

    /**
     * The auxiliary brake status element.
     */
    private AuxiliaryBrakeStatus auxBrakes;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public BrakeSystemStatus() {

        wheelBrakes = new BrakeAppliedStatus();
        traction = new TractionControlStatus();
        abs = new AntiLockBrakeStatus();
        scs = new StabilityControlStatus();
        brakeBoost = new BrakeBoostApplied();
        auxBrakes = new AuxiliaryBrakeStatus();
    }

    /**
     * A constructor for the brake system status.
     * 
     * @param wheelBrakes The brake applied status element.
     * @param traction The traction control status element.
     * @param abs The anti lock brake status element.
     * @param scs The stability control status element.
     * @param brakeBoost The brake boost applied status element.
     * @param auxBrakes The auxiliary brake status element.
     */
    public BrakeSystemStatus(BrakeAppliedStatus wheelBrakes, TractionControlStatus traction, AntiLockBrakeStatus abs, StabilityControlStatus scs, BrakeBoostApplied brakeBoost,
            AuxiliaryBrakeStatus auxBrakes) {

        this.wheelBrakes = Objects.requireNonNull(wheelBrakes);
        this.traction = Objects.requireNonNull(traction);
        this.abs = Objects.requireNonNull(abs);
        this.scs = Objects.requireNonNull(scs);
        this.brakeBoost = Objects.requireNonNull(brakeBoost);
        this.auxBrakes = Objects.requireNonNull(auxBrakes);
    }

    /**
     * A constructor for the brake system status which allows primitive data to be passed to all
     * primitive elements.
     * 
     * @param wheelBrakes The brake applied status element.
     * @param traction The traction control enumeration.
     * @param abs The anti lock brake enumeration.
     * @param scs The stability control enumeration.
     * @param brakeBoost The brake boost applied enumeration.
     * @param auxBrakes The auxiliary brake enumeration.
     */
    public BrakeSystemStatus(BrakeAppliedStatus wheelBrakes, TractionControl traction, AntiLockBrake abs, StabilityControl scs, BrakeBoost brakeBoost, AuxiliaryBrake auxBrakes) {

        this.wheelBrakes = Objects.requireNonNull(wheelBrakes);
        this.traction = new TractionControlStatus(Objects.requireNonNull(traction));
        this.abs = new AntiLockBrakeStatus(Objects.requireNonNull(abs));
        this.scs = new StabilityControlStatus(Objects.requireNonNull(scs));
        this.brakeBoost = new BrakeBoostApplied(Objects.requireNonNull(brakeBoost));
        this.auxBrakes = new AuxiliaryBrakeStatus(Objects.requireNonNull(auxBrakes));
    }

    /**
     * A getter for the brake applied status element.
     * 
     * @return The brake applied status element.
     */
    public BrakeAppliedStatus getWheelBrakes() {

        return wheelBrakes;
    }

    /**
     * A setter for the brake applied status element.
     * 
     * @param wheelBrakes The brake applied status element.
     */
    public void setWheelBrakes(BrakeAppliedStatus wheelBrakes) {

        this.wheelBrakes = Objects.requireNonNull(wheelBrakes);
    }

    /**
     * A getter for the traction control status element.
     * 
     * @return The traction control status element.
     */
    public TractionControlStatus getTraction() {

        return traction;
    }

    /**
     * A setter for the traction control status element.
     * 
     * @param traction The traction control status element to set.
     */
    public void setTraction(TractionControlStatus traction) {

        this.traction = Objects.requireNonNull(traction);
    }

    /**
     * A setter for the traction control status element. Allows enumeration data to be passed to the
     * enumeration element.
     * 
     * @param traction The traction control enumeration to be set in the element.
     */
    public void setTraction(TractionControl traction) {

        this.traction.setEnumeration(Objects.requireNonNull(traction));
    }

    /**
     * A getter for the anti lock brake status element.
     * 
     * @return The anti lock brake status element.
     */
    public AntiLockBrakeStatus getAbs() {

        return abs;
    }

    /**
     * A setter for the anti lock brake status element.
     * 
     * @param abs The anti lock brake status element to set.
     */
    public void setAbs(AntiLockBrakeStatus abs) {

        this.abs = Objects.requireNonNull(abs);
    }

    /**
     * A setter for the anti lock brake status element. Allows enumeration data to be passed to the
     * enumeration element.
     * 
     * @param abs The anti lock brake enumeration to be set in the element.
     */
    public void setAbs(AntiLockBrake abs) {

        this.abs.setEnumeration(Objects.requireNonNull(abs));
    }

    /**
     * A getter for the stability control status element.
     * 
     * @return The stability control status element.
     */
    public StabilityControlStatus getScs() {

        return scs;
    }

    /**
     * A setter for the stability control status element.
     * 
     * @param scs The stability control status element to set.
     */
    public void setScs(StabilityControlStatus scs) {

        this.scs = Objects.requireNonNull(scs);
    }

    /**
     * A setter for the stability control status element. Allows enumeration data to be passed to
     * the enumeration element.
     * 
     * @param scs The stability control enumeration to be set in the element.
     */
    public void setScs(StabilityControl scs) {

        this.scs.setEnumeration(Objects.requireNonNull(scs));
    }

    /**
     * A getter for the brake boost applied status element.
     * 
     * @return The brake boost applied status element.
     */
    public BrakeBoostApplied getBrakeBoost() {

        return brakeBoost;
    }

    /**
     * A setter for the brake boost applied status element.
     * 
     * @param brakeBoost The brake boost applied status element to set.
     */
    public void setBrakeBoost(BrakeBoostApplied brakeBoost) {

        this.brakeBoost = Objects.requireNonNull(brakeBoost);
    }

    /**
     * A setter for the brake boost applied status element. Allows enumeration data to be passed to
     * the enumeration element.
     * 
     * @param brakeBoost The brake boost enumeration to be set in the element.
     */
    public void setBrakeBoost(BrakeBoost brakeBoost) {

        this.brakeBoost.setEnumeration(Objects.requireNonNull(brakeBoost));
    }

    /**
     * A getter for the auxiliary brake status element.
     * 
     * @return The auxiliary brake status element.
     */
    public AuxiliaryBrakeStatus getAuxBrakes() {

        return auxBrakes;
    }

    /**
     * A setter for the auxiliary brake status element.
     * 
     * @param auxBrakes The auxiliary brake status element to set.
     */
    public void setAuxBrakes(AuxiliaryBrakeStatus auxBrakes) {

        this.auxBrakes = Objects.requireNonNull(auxBrakes);
    }

    /**
     * A setter for the auxiliary brake status element. Allows enumeration data to be passed to the
     * enumeration element.
     * 
     * @param auxBrakes The auxiliary brake enumeration to be set in the element.
     */
    public void setAuxBrakes(AuxiliaryBrake auxBrakes) {

        this.auxBrakes.setEnumeration(Objects.requireNonNull(auxBrakes));
    }

    @Override
    public String encodeUPER() {

        StringBuilder brakeSystemStatus = new StringBuilder();

        brakeSystemStatus.append(wheelBrakes.encodeUPER());
        brakeSystemStatus.append(traction.encodeUPER());
        brakeSystemStatus.append(abs.encodeUPER());
        brakeSystemStatus.append(scs.encodeUPER());
        brakeSystemStatus.append(brakeBoost.encodeUPER());
        brakeSystemStatus.append(auxBrakes.encodeUPER());

        return brakeSystemStatus.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = wheelBrakes.decodeUPER(bits);
        bits = traction.decodeUPER(bits);
        bits = abs.decodeUPER(bits);
        bits = scs.decodeUPER(bits);
        bits = brakeBoost.decodeUPER(bits);
        bits = auxBrakes.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(wheelBrakes, traction, abs, scs, brakeBoost, auxBrakes);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof BrakeSystemStatus)) {

            return false;
        }
        BrakeSystemStatus frame = (BrakeSystemStatus)object;
        return this.wheelBrakes.equals(frame.wheelBrakes)
                && this.traction.equals(frame.traction)
                && this.abs.equals(frame.abs)
                && this.scs.equals(frame.scs)
                && this.brakeBoost.equals(frame.brakeBoost)
                && this.auxBrakes.equals(frame.auxBrakes);
    }
}
