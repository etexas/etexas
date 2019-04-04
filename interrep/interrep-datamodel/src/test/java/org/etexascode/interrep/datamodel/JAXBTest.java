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
package org.etexascode.interrep.datamodel;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.etexascode.interrep.datamodel.LaneManager;
import org.etexascode.interrep.datamodel.xmladapters.ManagerAdapter.Entry;
import org.etexascode.interrep.datamodel.SignalIndication.Color;
import org.etexascode.interrep.datamodel.SignalIndication.State;
import org.etexascode.interrep.datamodel.SignalIndication.Type;
import org.etexascode.interrep.datamodel.interfaces.IDetectorManager;

/**
 * Test that the JAXB is able to correctly handle the interface approach to the datamodel.
 * 
 * @author cdeisher
 */
public class JAXBTest {

    public static void main(String[] args) {

        // changes made to other files to make this work-
        /**
         * InterRepInfoModel
         * 
         * @XmlElement(type = LaneManager.class) //added the type of the non interface right before
         *                  the interface is defined. With addition of ManagerAdaptorString, changed
         *                  XMLType names for inner classes so it doesn't get mad about two
         *                  different classes having same name NO LONGER RELEVENT BELOW, though
         *                  keeping until we have the webstart functioning added adaptor classes,
         *                  though they can be taken out if the interface equivalent comments out
         *                  javatype adaptor line ie /** An adaptor for JAXB to deal with interfaces
         * @author cdeisher public static class Adapter extends XmlAdapter<DetectorManager,
         *         IDetectorManager> { public IDetectorManager unmarshal(DetectorManager v) { return
         *         v; } public DetectorManager marshal(IDetectorManager v) { return
         *         (DetectorManager) v; } } changed at line 60 in ManagerAdaptor (in datamodel) //
         *         cdeisher- needed for JAXB, possibly because vehicles return the // string when we
         *         need an Integer here Integer key; if
         *         (entry.getValue().getClass().equals(Vehicle.class)) { Vehicle veh = (Vehicle)
         *         entry.getValue(); key = veh.getLaneID(); } else { key = entry.getKey(); }
         *         map.entry.add(new Entry<T>(key, entry.getValue()));
         */

        LaneManager lMan = new LaneManager();

        VehicleManager vMan = new VehicleManager();
        List<Vehicle> vehList = new ArrayList<Vehicle>();
        Vehicle v = new Vehicle();
        v.setBrakePressed(false);
        v.setHeading(1.0);
        v.setAcceleration(0.0);
        v.setLatitude(0.0);
        v.setLength(30);
        v.setLongitude(0);
        v.x = 0.0;
        v.y = 0.0;
        v.z = 0.0;
        v.setElev(0.0);
        v.setHeight(3.0);
        v.setLaneID(1);
        v.setSpeed(22.0);
        v.setType(Vehicle.VEHICLE_TYPE.CAR);
        v.setVehicleID(1);
        v.setWidth(3);
        vehList.add(v);
        vMan.addVehicle(v);
        // vMan.addVehicles(vehList);

        SignalManager sMan = new SignalManager();
        SignalIndication sig = new SignalIndication();
        sig.setLaneId(1);
        sig.setColorIndication(Color.GREEN);
        sig.setTimeToChange(5.0);
        // sig.setStateIndication(new State());
        sig.setTypeIndication(Type.BALL);
        sMan.addSignal(sig);

        DetectorManager dMan = new DetectorManager();
        Detector det = new Detector();
        det.setDetectorID(2);
        dMan.addDetector(2, det);

        ReferencePoint[] refPoint = new ReferencePoint[1];
        refPoint[0] = new ReferencePoint();
        refPoint[0].setLatitude(0.0);
        refPoint[0].setLongitude(1.0);

        double simTime = 0.0;

        double timeStep = 0.1;

        InterRepInfoModel irim = new InterRepInfoModel(lMan, vMan, sMan, dMan,
                refPoint, simTime, timeStep);

        try {
            File file = new File(
                    "C:\\Users\\cdeisher\\Desktop\\Extract\\file.xml");
            JAXBContext jaxbContext = JAXBContext
                    .newInstance(InterRepInfoModel.class);
            Marshaller jaxbMarshaller = jaxbContext.createMarshaller();

            jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);

            jaxbMarshaller.marshal(irim, file);
            jaxbMarshaller.marshal(irim, System.out);

            // unmarshall
            Unmarshaller unmar = jaxbContext.createUnmarshaller();

            InterRepInfoModel readIRIM = (InterRepInfoModel)unmar
                    .unmarshal(file);

            System.out.println("readIRIM\nLanes\n"
                    + readIRIM.getLmi().toString() + "\nVehicles\n"
                    + readIRIM.getVmi().toString() + "\nSignals\n"
                    + readIRIM.getSmi().toString() + "\nDetectors\n"
                    + readIRIM.getDmi().toString());

        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

}
