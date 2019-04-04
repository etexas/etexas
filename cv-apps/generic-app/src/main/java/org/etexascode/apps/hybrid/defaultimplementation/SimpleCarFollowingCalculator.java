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
package org.etexascode.apps.hybrid.defaultimplementation;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.etexascode.apps.hybrid.interfaces.ICarFollowingCalculator;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.Vehicle;
import org.etexascode.interrep.datamodel.interfaces.IDistanceable;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ILaneNode;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.utils.UtilsCalculations;
import org.etexascode.interrep.datamodel.utils.UtilsUnitConversion;

/**
 * Base implementation of the car following model. The paper we are basing this class off of is
 * 120726 - RPA Model Enhancement - TRB2013.pdf Note: I am ignoring the ability of cars to change
 * lanes in this module all together. The direction that the car moves will need to be refined in
 * order to take this into account.
 * 
 * @author ablatt
 */
public class SimpleCarFollowingCalculator implements ICarFollowingCalculator {

    double xc = 13.83; // velocity at capacity (critical velocity)

    double dltXc = 8.62 * 2; // vehicle spacing at capacity

    double dltXj = 8.62; // spacing at jam density

    double eta = 0.7; // driveline efficiency

    double gamma = 0.764; // vehicle throttle level 0.0 < gamma < 1.0

    double muTa = 55.0; // % mass of vehicle on tractive axle

    double mu = 0.6; // coefficient of roadway adhesion

    double cd = 0.36; // drag coeficient

    double cr = 1.25; // rolling coeficient

    double cr2 = 0.0328; // rolling resistance coefficient

    double cr3 = 4.575; // rolling resistance coefficient -- this one appears to have been

    // mislabled in his paper as Constant accounting for density of air at sea level
    // we can only hope that this is the variable he was refering to
    // NOTE: ablatt - the rolling coefficients were taken from a different paper.
    // As such, their ordering and meaning could be wrong.
    // Dr. Rakha has not been helpful thus far in that respect

    // needs to be calculated from incoming information
    private double xdMin = 4.481; // minimum acceleration or max

    double rho = 90.0; // vehicle power

    double af = 2.06; // vehicle frontal area

    double vehMass = 1.19; // default mass of a vehicle

    InterRepInfoModel prevModel = null;

    static final double halfMax = Double.MAX_VALUE / 2.0; // half chosen so we don't get any
                                                          // overflow issues

    @Override
    public Map<Integer, List<Vehicle>> performCarFollowing(Map<Integer, List<Vehicle>> vehiclesByLane, InterRepInfoModel interRep) {
        if (prevModel != null) {
            for (Entry<Integer, List<Vehicle>> entry : vehiclesByLane.entrySet()) {
                List<Vehicle> currVehs = entry.getValue();
                List<? extends IVehicle> curr = interRep.vmi.getVehiclesInLane(entry.getKey());
                ILane li = interRep.lmi.getLaneById(entry.getKey());

                for (Vehicle v : currVehs) {
                    IVehicle prev = prevModel.vmi.getVehicle(v.getProperId());
                    if (prev != null) {
                        moveVehicle(v, li, interRep.simTime - prevModel.simTime);
                        IVehicle vi = getInFront(v, curr);
                        if ((vi == null) || over50sAway(v, vi)) {
                            /*
                             * v.setSpeed(eq9(interRep.lmi.getILaneById(entry.getKey
                             * ()).getSpeedLimitInMetersPerSecond(),
                             * interRep.lmi.getILaneById(entry.getKey()).
                             * getSpeedLimitInMetersPerSecond() * 60, vehMass, interRep.simTime -
                             * prevModel.simTime, interRep.lmi.getILaneById(entry.getKey()).
                             * getSpeedLimitInMetersPerSecond()));
                             */
                            v.setSpeed(interRep.lmi.getLaneById(entry.getKey()).getSpeedLimitInMetersPerSecond());
                        }
                        else {
                            v.setSpeed(eq9(vi.getSpeed(), UtilsCalculations.getDistance(v, vi), vehMass, interRep.simTime - prevModel.simTime, interRep.lmi.getLaneById(entry.getKey())
                                    .getSpeedLimitInMetersPerSecond()));
                        }
                    }
                }
            }
        }

        prevModel = interRep;

        return vehiclesByLane;
    }

    boolean over50sAway(Vehicle v, IVehicle vi) {
        return UtilsCalculations.getDistance(v, vi) / UtilsUnitConversion.convertMetersToCentimeters(v.getSpeed()) > 50.0;
    }

    IVehicle getInFront(Vehicle nP1, List<? extends IVehicle> vehs) {
        IVehicle ret = null;

        for (IVehicle v : vehs) {
            if (v.getVehicleID() == nP1.getVehicleID()) {
                break;
            }
            else {
                ret = v;
            }
        }

        return ret;
    }

    /**
     * @param v
     * @param li
     * @param moveTime
     */
    void moveVehicle(Vehicle v, ILane li, double moveTime) {
        ILaneNode lni = null;
        double d = Math.abs(UtilsCalculations.getDistance(v, li));
        for (ILaneNode tmp : li.getLaneGeomList()) {
            if (d > Math.abs(UtilsCalculations.getDistance(tmp, li))) {
                lni = tmp;
            }
            else {
                break;
            }
        }

        performMove(v, lni, moveTime);
    }

    /**
     * Move the vehicle toward the target.
     * 
     * @param v The vehicle to move.
     * @param target The point to move the vehicle toward.
     * @param moveTime The time over which the vehicle is moving (multiplied with the speed of the
     *        vehicle to determine how far to move the vehicle)
     */
    void performMove(Vehicle v, IDistanceable target, double moveTime) {
        double[] d = UtilsCalculations.moveToward(v, target, UtilsUnitConversion.convertMetersToCentimeters(v.getSpeed() * moveTime));
        v.setX(d[0]);
        v.setY(d[1]);
    }

    /**
     * Implementation of equation 9 in 120726 - RPA Model Enhancement - TRB2013.pdf
     * 
     * @param xn
     * @param xnPrev
     * @param vehMass
     * @param dt
     * @return The speed of the vehicle on the next time step.
     */
    double eq9(double xn, double dDistVehs, double vehMass, double dt, double xd) {
        double f1 = FnPlus1(eta, gamma, rho, xn, muTa, mu);
        double f2 = FnPlus1(eta, 0.0, rho, xn, muTa, mu);
        double r = RnPlus1(cd, af, xn, vehMass, cr, cr2, cr3);
        double e3 = eq3(xc, xd, dltXj, dltXc, dDistVehs);
        double e4 = eq4(xn, xdMin, dDistVehs, dltXj);
        double e5 = eq5(xn, dt, vehMass, f1, r);
        double e5Gam0 = eq5(xn, dt, vehMass, f2, r);
        return Math.max(Math.min(e3, Math.min(e4, e5)), Math.min(e4, e5Gam0));
    }

    /**
     * Implementation of the third equation in the specified paper.
     * 
     * @param xc
     * @param xd
     * @param dxj
     * @param dxc
     * @param dxn
     * @return The eq3 value.
     */
    double eq3(double xc, double xd, double dxj, double dxc, double dxn) {
        double sub = subC1(xc, xd, dxj);
        double c1t = c1(xc, xd, dxj, sub);
        double c2t = c2(xc, xd, dxj, sub);
        double c3t = c3(xc, xd, dxj, dxc, sub);
        double p1 = (c3t * xd) - c1t + dxn;
        double p2 = p1 * p1;
        double p3 = (dxn * xd) - (c1t * xd) - c2t;
        double p4 = 4 * c3t * p3;
        double p5 = Math.sqrt(p2 - p4);
        return (p1 - p5) / (2 * c3t);
    }

    /**
     * Implementation of the forth equation in the specified paper.
     * 
     * @param xn
     * @param xdmin
     * @param dxn
     * @param dxj
     * @return The eq4 value.
     */
    double eq4(double xn, double xdmin, double dxn, double dxj) {
        return Math.sqrt((xn * xn) + (2 * xdmin * (dxn - dxj)));
    }

    /**
     * Implementation of the fifth equation in the specified paper.
     * 
     * @param xn
     * @param dt
     * @param vehMass
     * @param f
     * @param r
     * @return The eq5 value.
     */
    double eq5(double xn, double dt, double vehMass, double f, double r) {
        return xn + (dt * ((f - r) / vehMass));
    }

    /**
     * Implementation of the C1 equation from the specified paper.
     * 
     * @param xc
     * @param xd
     * @param dxj
     * @param sub
     * @return the c1 value.
     */
    double c1(double xc, double xd, double dxj, double sub) {
        return sub * ((2 * xc) - xd);
    }

    /**
     * Implementation of the C2 equation from the specified paper.
     * 
     * @param xc
     * @param xd
     * @param dxj
     * @param sub
     * @return The c2 value.
     */
    double c2(double xc, double xd, double dxj, double sub) {
        return sub * (Math.pow((xd - xc), 2.0));
    }

    /**
     * Implementation of the C3 equation from the specified paper.
     * 
     * @param xc
     * @param xd
     * @param dxj
     * @param dxc
     * @param sub
     * @return The c3 value.
     */
    double c3(double xc, double xd, double dxj, double dxc, double sub) {
        return (dxc / xc) - sub;
    }

    /**
     * Utility funciton used in each of the C functions.
     * 
     * @param xc
     * @param xd
     * @param dxj
     * @return The subc1 value.
     */
    double subC1(double xc, double xd, double dxj) {
        return (dxj * xd) / (xc * xc);
    }

    /**
     * Implementation of the Fn+1 equation in the specified paper.
     * 
     * @param eta
     * @param gamma
     * @param rho
     * @param xn
     * @param muTa
     * @param mu
     * @return The fn plus 1 value.
     */
    double FnPlus1(double eta, double gamma, double rho, double xn, double muTa, double mu) {
        return Math.min((3600 * eta * ((gamma * rho) / xn)), (9.8066 * muTa * mu));
    }

    /**
     * Implementation of the Rn+1 equation in the specified paper.
     * 
     * @param cd
     * @param af
     * @param xn
     * @param vehMass
     * @param cr
     * @param cr2
     * @param cr3
     * @return The rn plus 1 value.
     */
    double RnPlus1(double cd, double af, double xn, double vehMass, double cr, double cr2, double cr3) {
        double p1 = 0.047285 * cd * af * xn * xn;
        double p2 = 9.8066 * vehMass * cr;
        double p3 = (cr2 * xn) + cr3;
        return p1 + (p2 * p3);
    }
}
