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
package org.etexascode.datalayer.inmemory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.etexascode.datalayer.interfaces.IIntersectionModelsComponent;
import org.etexascode.interrep.datamodel.InterRepInfoModel;
import org.etexascode.interrep.datamodel.VehicleManager;
import org.etexascode.interrep.datamodel.interfaces.IDetector;
import org.etexascode.interrep.datamodel.interfaces.ILane;
import org.etexascode.interrep.datamodel.interfaces.ISignalIndication;
import org.etexascode.interrep.datamodel.interfaces.IVehicle;
import org.etexascode.interrep.datamodel.interfaces.IVehicleManager;

/**
 * The default implementation of the container to access <code>InterRepInfoModel</code> models.
 * 
 * @author emyers
 */
public class DefaultIntersectionModelsComponent implements IIntersectionModelsComponent {

    /** The map of intersection information models. */
    private Map<Integer, InterRepInfoModel> modelMap;

    /**
     * Creates a new <code>DefaultIntersectionModelsComponent</code>.
     */
    public DefaultIntersectionModelsComponent() {

        modelMap = new HashMap<Integer, InterRepInfoModel>();
    }

    @Override
    public void putInterRepInfoModel(int stepNum, int interRepId, InterRepInfoModel model) {

        modelMap.put(interRepId, model);
    }

    @Override
    public InterRepInfoModel getInfoModel(int stepNum, int interRepId) {

        return modelMap.get(interRepId);
    }

    @Override
    public IVehicleManager getVehicles(int stepNum, int interRepId) {

        InterRepInfoModel model = modelMap.get(interRepId);
        return (model != null) ? model.vmi : new VehicleManager();
    }

    @Override
    public IVehicle getVehicleInfoById(int stepNum, String id) {

        for (InterRepInfoModel model : modelMap.values()) {

            if (model.vmi.getVehicle(id) != null) {

                return model.vmi.getVehicle(id);
            }
        }

        return null;
    }

    @Override
    public ILane getLaneInfoByVehicle(int stepNum, IVehicle vi) {

        for (InterRepInfoModel model : modelMap.values()) {

            if (model.lmi.getLaneById(vi.getLaneID()) != null) {

                return model.lmi.getLaneById(vi.getLaneID());
            }
        }

        return null;
    }

    @Override
    public List<? extends ISignalIndication> getSignalsByLane(int stepNum, ILane li) {

        for (InterRepInfoModel model : modelMap.values()) {

            if (model.smi.getSignalsByLaneId(li.getLaneId()) != null) {

                return model.smi.getSignalsByLaneId(li.getLaneId());
            }
        }

        return null;
    }

    @Override
    public Iterable<? extends IDetector> getDetectors(int stepNum, int interRepId) {

        InterRepInfoModel model = modelMap.get(interRepId);
        return (model != null && model.dmi != null) ? model.dmi : new ArrayList<IDetector>();
    }
}
