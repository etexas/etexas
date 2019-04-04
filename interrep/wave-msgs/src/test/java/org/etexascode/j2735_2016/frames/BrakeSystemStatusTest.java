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
package org.etexascode.j2735_2016.frames;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for the brake system status frame.
 * 
 * @author ttevendale
 */
public class BrakeSystemStatusTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    BrakeSystemStatus brakeSystemStatus;

    String encodedBits;

    @Before
    public void init() {

        BrakeAppliedStatus wheelBrakes = new BrakeAppliedStatus();
        wheelBrakes.setLeftFront(true);
        TractionControlStatus traction = new TractionControlStatus(TractionControl.OFF);
        AntiLockBrakeStatus abs = new AntiLockBrakeStatus(AntiLockBrake.ENGAGED);
        StabilityControlStatus scs = new StabilityControlStatus(StabilityControl.UNAVAILABLE);
        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.OFF);
        AuxiliaryBrakeStatus auxBrakes = new AuxiliaryBrakeStatus(AuxiliaryBrake.RESERVED);

        brakeSystemStatus = new BrakeSystemStatus(wheelBrakes, traction, abs, scs, brakeBoost, auxBrakes);
        encodedBits = wheelBrakes.encodeUPER() + traction.encodeUPER() + abs.encodeUPER() + scs.encodeUPER() + brakeBoost.encodeUPER() + auxBrakes.encodeUPER();
    }

    @Test
    public void testConstructor() {

        BrakeAppliedStatus wheelBrakes = new BrakeAppliedStatus();
        wheelBrakes.setLeftRear(true);
        TractionControlStatus traction = new TractionControlStatus(TractionControl.ON);
        AntiLockBrakeStatus abs = new AntiLockBrakeStatus(AntiLockBrake.ENGAGED);
        StabilityControlStatus scs = new StabilityControlStatus(StabilityControl.OFF);
        BrakeBoostApplied brakeBoost = new BrakeBoostApplied(BrakeBoost.UNAVAILABLE);
        AuxiliaryBrakeStatus auxBrakes = new AuxiliaryBrakeStatus(AuxiliaryBrake.OFF);

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus(wheelBrakes, traction, abs, scs, brakeBoost, auxBrakes);

        assertTrue(wheelBrakes.equals(brakeSystemStatus.getWheelBrakes()));
        assertTrue(traction.equals(brakeSystemStatus.getTraction()));
        assertTrue(abs.equals(brakeSystemStatus.getAbs()));
        assertTrue(scs.equals(brakeSystemStatus.getScs()));
        assertTrue(brakeBoost.equals(brakeSystemStatus.getBrakeBoost()));
        assertTrue(auxBrakes.equals(brakeSystemStatus.getAuxBrakes()));
    }

    @Test
    public void testConstructorNullWheelBrakes() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(null, new TractionControlStatus(), new AntiLockBrakeStatus(), new StabilityControlStatus(), new BrakeBoostApplied(), new AuxiliaryBrakeStatus());
    }

    @Test
    public void testConstructorNullTraction() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), null, new AntiLockBrakeStatus(), new StabilityControlStatus(), new BrakeBoostApplied(), new AuxiliaryBrakeStatus());
    }

    @Test
    public void testConstructorNullAbs() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), new TractionControlStatus(), null, new StabilityControlStatus(), new BrakeBoostApplied(), new AuxiliaryBrakeStatus());
    }

    @Test
    public void testConstructorNullScs() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), new TractionControlStatus(), new AntiLockBrakeStatus(), null, new BrakeBoostApplied(), new AuxiliaryBrakeStatus());
    }

    @Test
    public void testConstructorNullBrakeBoost() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), new TractionControlStatus(), new AntiLockBrakeStatus(), new StabilityControlStatus(), null, new AuxiliaryBrakeStatus());
    }

    @Test
    public void testConstructorNullAuxBrakes() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), new TractionControlStatus(), new AntiLockBrakeStatus(), new StabilityControlStatus(), new BrakeBoostApplied(), null);
    }

    @Test
    public void testConstructorPrimitive() {

        BrakeAppliedStatus wheelBrakes = new BrakeAppliedStatus();
        wheelBrakes.setLeftRear(true);
        TractionControl traction = TractionControl.ON;
        AntiLockBrake abs = AntiLockBrake.ENGAGED;
        StabilityControl scs = StabilityControl.OFF;
        BrakeBoost brakeBoost = BrakeBoost.UNAVAILABLE;
        AuxiliaryBrake auxBrakes = AuxiliaryBrake.OFF;

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus(wheelBrakes, traction, abs, scs, brakeBoost, auxBrakes);

        assertTrue(wheelBrakes.equals(brakeSystemStatus.getWheelBrakes()));
        assertTrue(traction.equals(brakeSystemStatus.getTraction().getEnumeration()));
        assertTrue(abs.equals(brakeSystemStatus.getAbs().getEnumeration()));
        assertTrue(scs.equals(brakeSystemStatus.getScs().getEnumeration()));
        assertTrue(brakeBoost.equals(brakeSystemStatus.getBrakeBoost().getEnumeration()));
        assertTrue(auxBrakes.equals(brakeSystemStatus.getAuxBrakes().getEnumeration()));
    }

    @Test
    public void testConstructorPrimitiveNullWheelBrakes() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(null, TractionControl.OFF, AntiLockBrake.OFF, StabilityControl.OFF, BrakeBoost.OFF, AuxiliaryBrake.OFF);
    }

    @Test
    public void testConstructorPrimitiveNullTraction() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), null, AntiLockBrake.OFF, StabilityControl.OFF, BrakeBoost.OFF, AuxiliaryBrake.OFF);
    }

    @Test
    public void testConstructorPrimitiveNullAbs() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.OFF, null, StabilityControl.OFF, BrakeBoost.OFF, AuxiliaryBrake.OFF);
    }

    @Test
    public void testConstructorPrimitiveNullScs() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.OFF, AntiLockBrake.OFF, null, BrakeBoost.OFF, AuxiliaryBrake.OFF);
    }

    @Test
    public void testConstructorPrimitiveNullbrakeBoost() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.OFF, AntiLockBrake.OFF, StabilityControl.OFF, null, AuxiliaryBrake.OFF);
    }

    @Test
    public void testConstructorPrimitiveNullAuxBrakes() {

        thrown.expect(NullPointerException.class);
        new BrakeSystemStatus(new BrakeAppliedStatus(), TractionControl.OFF, AntiLockBrake.OFF, StabilityControl.OFF, BrakeBoost.OFF, null);
    }

    @Test
    public void testSetWheelBrakes() {

        BrakeAppliedStatus wheelBrakes = new BrakeAppliedStatus();
        wheelBrakes.setLeftFront(true);

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setWheelBrakes(wheelBrakes);

        assertTrue(wheelBrakes.equals(brakeSystemStatus.getWheelBrakes()));

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setWheelBrakes(null);
    }

    @Test
    public void testSetTraction() {

        TractionControlStatus traction = new TractionControlStatus();

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setTraction(traction);

        assertTrue(traction.equals(brakeSystemStatus.getTraction()));

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setTraction((TractionControlStatus)null);
    }

    @Test
    public void testSetTractionPrimitive() {

        TractionControl traction = TractionControl.ON;

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setTraction(traction);

        assertTrue(traction == brakeSystemStatus.getTraction().getEnumeration());

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setTraction((TractionControl)null);
    }

    @Test
    public void testSetAbs() {

        AntiLockBrakeStatus abs = new AntiLockBrakeStatus();

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setAbs(abs);

        assertTrue(abs.equals(brakeSystemStatus.getAbs()));

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setAbs((AntiLockBrakeStatus)null);
    }

    @Test
    public void testSetAbsPrimitive() {

        AntiLockBrake abs = AntiLockBrake.OFF;

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setAbs(abs);

        assertTrue(abs == brakeSystemStatus.getAbs().getEnumeration());

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setAbs((AntiLockBrake)null);
    }

    @Test
    public void testSetScs() {

        StabilityControlStatus scs = new StabilityControlStatus();

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setScs(scs);

        assertTrue(scs.equals(brakeSystemStatus.getScs()));

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setScs((StabilityControlStatus)null);
    }

    @Test
    public void testSetScsPrimitive() {

        StabilityControl scs = StabilityControl.ENGAGED;

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setScs(scs);

        assertTrue(scs == brakeSystemStatus.getScs().getEnumeration());

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setScs((StabilityControl)null);
    }

    @Test
    public void testSetBrakeBoost() {

        BrakeBoostApplied brakeBoost = new BrakeBoostApplied();

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setBrakeBoost(brakeBoost);

        assertTrue(brakeBoost.equals(brakeSystemStatus.getBrakeBoost()));

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setBrakeBoost((BrakeBoostApplied)null);
    }

    @Test
    public void testSetBrakeBoostPrimitive() {

        BrakeBoost brakeBoost = BrakeBoost.UNAVAILABLE;

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setBrakeBoost(brakeBoost);

        assertTrue(brakeBoost == brakeSystemStatus.getBrakeBoost().getEnumeration());

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setBrakeBoost((BrakeBoost)null);
    }

    @Test
    public void testSetAuxBrakes() {

        AuxiliaryBrakeStatus auxBrakes = new AuxiliaryBrakeStatus();

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setAuxBrakes(auxBrakes);

        assertTrue(auxBrakes.equals(brakeSystemStatus.getAuxBrakes()));

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setAuxBrakes((AuxiliaryBrakeStatus)null);
    }

    @Test
    public void testSetAuxBrakesPrimitive() {

        AuxiliaryBrake auxBrakes = AuxiliaryBrake.RESERVED;

        BrakeSystemStatus brakeSystemStatus = new BrakeSystemStatus();
        brakeSystemStatus.setAuxBrakes(auxBrakes);

        assertTrue(auxBrakes == brakeSystemStatus.getAuxBrakes().getEnumeration());

        thrown.expect(NullPointerException.class);
        brakeSystemStatus.setAuxBrakes((AuxiliaryBrake)null);
    }

    @Test
    public void testEncodeUPER() {

        assertTrue(encodedBits.equalsIgnoreCase(brakeSystemStatus.encodeUPER()));
    }

    @Test
    public void testDecodeUPER() {

        BrakeSystemStatus decodedBrakeSystemStatus = new BrakeSystemStatus();
        decodedBrakeSystemStatus.decodeUPER(encodedBits);
        assertTrue(brakeSystemStatus.equals(decodedBrakeSystemStatus));
    }

    @Test
    public void testHashCode() {

        BrakeAppliedStatus brakeAppliedStatus = brakeSystemStatus.getWheelBrakes();
        TractionControl traction = brakeSystemStatus.getTraction().getEnumeration();
        AntiLockBrake abs = brakeSystemStatus.getAbs().getEnumeration();
        StabilityControl scs = brakeSystemStatus.getScs().getEnumeration();
        BrakeBoost brakeBoost = brakeSystemStatus.getBrakeBoost().getEnumeration();
        AuxiliaryBrake auxBrakes = brakeSystemStatus.getAuxBrakes().getEnumeration();

        BrakeAppliedStatus brakeAppliedStatus2 = new BrakeAppliedStatus();
        brakeAppliedStatus2.setLeftFront(!brakeSystemStatus.getWheelBrakes().isLeftFront());

        BrakeSystemStatus brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus2, traction, abs, scs, brakeBoost, auxBrakes);

        assertFalse(brakeSystemStatus.hashCode() == brakeSystemStatus2.hashCode());
        assertTrue(brakeSystemStatus.hashCode() == brakeSystemStatus.hashCode());
        assertTrue(brakeSystemStatus2.hashCode() == brakeSystemStatus2.hashCode());

        BrakeSystemStatus brakeSystemStatus3 = new BrakeSystemStatus(brakeAppliedStatus, traction, abs, scs, brakeBoost, auxBrakes);

        assertTrue(brakeSystemStatus.hashCode() == brakeSystemStatus3.hashCode());
        assertFalse(brakeSystemStatus2.hashCode() == brakeSystemStatus3.hashCode());
    }

    @Test
    public void testEquals() {

        assertTrue(brakeSystemStatus.equals(brakeSystemStatus));
        assertFalse(brakeSystemStatus.equals(null));
        assertFalse(brakeSystemStatus.equals(new String()));

        BrakeAppliedStatus brakeAppliedStatus = brakeSystemStatus.getWheelBrakes();
        TractionControl traction = brakeSystemStatus.getTraction().getEnumeration();
        AntiLockBrake abs = brakeSystemStatus.getAbs().getEnumeration();
        StabilityControl scs = brakeSystemStatus.getScs().getEnumeration();
        BrakeBoost brakeBoost = brakeSystemStatus.getBrakeBoost().getEnumeration();
        AuxiliaryBrake auxBrakes = brakeSystemStatus.getAuxBrakes().getEnumeration();

        BrakeSystemStatus brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus, traction, abs, scs, brakeBoost, auxBrakes);

        BrakeAppliedStatus brakeAppliedStatus2 = new BrakeAppliedStatus();
        brakeAppliedStatus2.setLeftFront(!brakeSystemStatus.getWheelBrakes().isLeftFront());

        // different
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus2, TractionControl.ENGAGED, AntiLockBrake.UNAVAILABLE, StabilityControl.ENGAGED, BrakeBoost.ON, AuxiliaryBrake.ON);

        assertFalse(brakeSystemStatus.equals(brakeSystemStatus2));

        // different brake applied status
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus2, traction, abs, scs, brakeBoost, auxBrakes);

        assertFalse(brakeSystemStatus.equals(brakeSystemStatus2));

        // different traction
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus, TractionControl.ENGAGED, abs, scs, brakeBoost, auxBrakes);

        assertFalse(brakeSystemStatus.equals(brakeSystemStatus2));

        // different abs
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus, traction, AntiLockBrake.UNAVAILABLE, scs, brakeBoost, auxBrakes);

        assertFalse(brakeSystemStatus.equals(brakeSystemStatus2));

        // different scs
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus, traction, abs, StabilityControl.ENGAGED, brakeBoost, auxBrakes);

        assertFalse(brakeSystemStatus.equals(brakeSystemStatus2));

        // different brake boost
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus, traction, abs, scs, BrakeBoost.ON, auxBrakes);

        assertFalse(brakeSystemStatus.equals(brakeSystemStatus2));

        // different auxiliary brake
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus, traction, abs, scs, brakeBoost, AuxiliaryBrake.ON);

        assertFalse(brakeSystemStatus.equals(brakeSystemStatus2));

        // same
        brakeSystemStatus2 = new BrakeSystemStatus(brakeAppliedStatus, traction, abs, scs, brakeBoost, auxBrakes);

        assertTrue(brakeSystemStatus.equals(brakeSystemStatus2));
    }
}
