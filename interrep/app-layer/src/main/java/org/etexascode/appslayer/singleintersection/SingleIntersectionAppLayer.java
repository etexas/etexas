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
package org.etexascode.appslayer.singleintersection;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.etexascode.apps.AppLoggerImpl;
import org.etexascode.apps.CellularDevice;
import org.etexascode.apps.ICellularBaseApp;
import org.etexascode.apps.IOBUBaseApp;
import org.etexascode.apps.IRSEBaseApp;
import org.etexascode.apps.IReportBaseApp;
import org.etexascode.apps.OBUDevice;
import org.etexascode.apps.RSEDevice;
import org.etexascode.apps.ReportDevice;
import org.etexascode.appslayer.IAppLayer;
import org.etexascode.appslayer.INativeAppManager;
import org.etexascode.appslayer.IRemoteAppLayer;
import org.etexascode.appslayer.IRemoteAppManager;
import org.etexascode.appslayerdata.AppLayerInput;
import org.etexascode.appslayerdata.AppLayerOutput;
import org.etexascode.appslayerdata.CellularDeviceInfo;
import org.etexascode.appslayerdata.IDeviceInfo;
import org.etexascode.appslayerdata.OBUDeviceInfo;
import org.etexascode.appslayerdata.RSEDeviceInfo;
import org.etexascode.appslayerdata.RemoteProxyApp;
import org.etexascode.appslayerdata.ReportDeviceInfo;
import org.etexascode.datalayer.interfaces.IAppsDataLayer;
import org.etexascode.datalayer.interfaces.IRestDataLayer;
import org.etexascode.devicedata.BasicMessage;
import org.etexascode.devicedata.IAppName;
import org.etexascode.devicedata.NativeApp;
import org.etexascode.driver.SimDriverException;
import org.etexascode.interrep.datamodel.SignalCommand;
import org.etexascode.interrep.datamodel.VehicleCommand;
import org.etexascode.wavesim.Tx.MessageType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An implementation of the apps layer designed for the single intersection use case. This could be
 * reused for a small number of intersections.
 * 
 * @author ablatt
 * @author bmauldon
 * @author jrutherford
 * @author janway
 */
public class SingleIntersectionAppLayer implements IAppLayer, IRemoteAppLayer {

    /**
     * Static logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(SingleIntersectionAppLayer.class);

    /**
     * The data layer to utilize throughout the process
     */
    final IAppsDataLayer data;

    /**
     * The remote app manager.
     */
    final IRemoteAppManager remoteAppManager;

    /**
     * The native app manager.
     */
    final INativeAppManager nativeAppManager;

    /**
     * Basic constructor.
     * 
     * @param layer The data layer to use
     * @param ram The remote app manager
     * @param nam The native app manager
     */
    public SingleIntersectionAppLayer(IAppsDataLayer layer, IRemoteAppManager ram, INativeAppManager nam) {
        data = layer;
        remoteAppManager = ram;
        nativeAppManager = nam;
    }

    /**
     * Takes in AppLayerInput and for execution of apps and converts to outputs
     * 
     * @param stepNum integer to represent the time step number of execution
     * @throws SimDriverException exception sent to logs
     */
    @Override
    public void execApps(int stepNum) {
        List<AppLayerInput> inputs = data.getAppData(stepNum);
        List<AppLayerOutput> outputs = new ArrayList<AppLayerOutput>(inputs.size());
        StringBuilder errorMsg = new StringBuilder();

        startNativeApps(inputs, data.getSimTime(stepNum));

        for (AppLayerInput ad : inputs) {
            try {
                outputs.add(processApp(ad, data.getSimTime(stepNum)));
            }
            catch (Exception e) {
                LOGGER.error("App Update Exception Detected and Caught", e);

                errorMsg.append("Error updating ");
                if (ad.app instanceof IAppName) {
                    try {
                        errorMsg.append(((IAppName)ad.app).getAppName());
                    }
                    catch (Exception e2) {
                        errorMsg.append(ad.app.getClass().getSimpleName());
                    }
                }
                else {
                    errorMsg.append(ad.app.getClass().getSimpleName());
                }
                errorMsg.append(". with error ");
                errorMsg.append(ExceptionUtils.getStackTrace(e));
            }
        }
        outputs.addAll(execApps(inputs));
        outputs.addAll(getNativeAppsOutput());
        data.putAppOutputs(stepNum, outputs);

        // Put the started and stopped remote applications in the app manager.
        if (data instanceof IRestDataLayer) {
            IRestDataLayer rdl = (IRestDataLayer)data;
            remoteAppManager.putStartedApps(rdl.getStartedRemoteApps(stepNum));
            remoteAppManager.putFinishedApps(rdl.getStoppedRemoteApps(stepNum));
        }

        if (errorMsg.toString().length() > 0) {
            throw new SimDriverException("Error updating apps", errorMsg.toString());

        }
    }

    /**
     * Executes an app and returns its output.
     * 
     * @param ad The app input.
     * @param simtime The sim time.
     * @return The app output.
     */
    @SuppressWarnings("unchecked")
    AppLayerOutput processApp(AppLayerInput ad, Double simtime) {
        String appName = "";
        if (ad.app instanceof IAppName) {
            appName = ((IAppName)ad.app).getAppName();
        }
        else {
            appName = ad.app.getClass().getSimpleName();
        }

        IDeviceInfo info = ad.input;
        Collection<BasicMessage> receive = info.getMessages();
        Object[] messages = getObjectArray(receive);

        AppLoggerImpl logger = new AppLoggerImpl(info.getDeviceId(), appName, simtime.doubleValue());
        AppLayerOutput ret = new AppLayerOutput(appName, info.getDeviceId(), info.getLocation());

        if (ad.app instanceof IOBUBaseApp) {
            IOBUBaseApp app = (IOBUBaseApp)ad.app;
            OBUDevice dev = new OBUDevice((OBUDeviceInfo)ad.input);
            app.performUpdate(dev, messages, receive, simtime, logger);
            ret.addMessagesList(dev.getAppMessages());
            dev.resetAppMessages();
            ret.addVehicleCommandsList(dev.getOutputCommands());
            ret.addSignalCommandsList(new ArrayList<SignalCommand>(0));
            ret.setMessageType(MessageType.DSRC);
        }
        else if (ad.app instanceof ICellularBaseApp) {
            ICellularBaseApp app = (ICellularBaseApp)ad.app;
            CellularDevice dev = new CellularDevice((CellularDeviceInfo)ad.input);
            app.performUpdate(dev, messages, receive, simtime, logger);
            ret.addMessagesList(dev.getAppMessages());
            dev.resetAppMessages();
            ret.addVehicleCommandsList(new ArrayList<VehicleCommand>(0));
            ret.addSignalCommandsList(new ArrayList<SignalCommand>(0));
            ret.setMessageType(MessageType.CELLULAR);
        }
        else if (ad.app instanceof IRSEBaseApp) {
            IRSEBaseApp app = (IRSEBaseApp)ad.app;
            RSEDevice dev = new RSEDevice((RSEDeviceInfo)ad.input);
            app.performUpdate(dev, messages, receive, simtime, logger);
            ret.addMessagesList(dev.getAppMessages());
            dev.resetAppMessages();
            ret.addSignalCommandsList(dev.getSignalCommands());
            ret.addVehicleCommandsList(new ArrayList<VehicleCommand>(0));
            ret.setMessageType(MessageType.DSRC);
        }
        else if (ad.app instanceof IReportBaseApp) {
            IReportBaseApp app = (IReportBaseApp)ad.app;
            ReportDevice dev = new ReportDevice((ReportDeviceInfo)ad.input);
            app.performUpdate(dev, messages, receive, simtime, logger);
            ret.addMessagesList(dev.getAppMessages());
            dev.resetAppMessages();
            ret.addSignalCommandsList(new ArrayList<SignalCommand>(0));
            ret.addVehicleCommandsList(new ArrayList<VehicleCommand>(0));
        }
        else {
            ad.app.performUpdate(info, messages, receive, simtime, logger);
        }

        if (ret.getOutMessages() == null) {
            ret.addMessagesList(new ArrayList<BasicMessage>(0));
        }

        ret.addLogsList(logger.getLogs());

        for (BasicMessage mr : ret.getOutMessages()) {
            mr.setOriginMACAddress(ret.mac);
        }

        return ret;
    }

    /**
     * Extract the messages from the indication array.
     * 
     * @param receive The indications to extract from.
     * @return The messages in the indications.
     */
    Object[] getObjectArray(Collection<BasicMessage> receive) {
        Object[] messages = new Object[receive.size()];
        int i = 0;
        for (BasicMessage wsmIndication : receive) {
            messages[i] = wsmIndication.getData();
            i++;
        }
        return messages;
    }

    /**
     * Executes a step by discharging all of the remote apps.
     * 
     * @param apps The list of apps.
     * @return The output for all remote apps.
     */
    @SuppressWarnings("rawtypes")
    public List<AppLayerOutput> execApps(List<AppLayerInput> apps) {
        List<AppLayerOutput> listALO = new ArrayList<AppLayerOutput>();
        for (AppLayerInput ali : apps) {
            if (ali.app instanceof RemoteProxyApp) {
                listALO.add(((RemoteProxyApp)ali.app).discharge());
            }
        }
        return listALO;
    }

    /**
     * Sends data to the native app manager so the native apps can start producing output for this
     * step.
     * 
     * @param apps The apps to execute.
     * @param simTime The sim time.
     */
    private void startNativeApps(List<AppLayerInput> apps, double simTime) {
        List<AppLayerInput> nativeApps = new LinkedList<AppLayerInput>();
        for (AppLayerInput ali : apps) {
            if (ali.app instanceof NativeApp) {
                nativeApps.add(ali);
            }
        }

        nativeAppManager.sendInputs(nativeApps, simTime);
    }

    /**
     * Gets the output produced by native apps from the native app manager.
     * 
     * @return The native apps' output.
     */
    private List<AppLayerOutput> getNativeAppsOutput() {
        return nativeAppManager.getOutputs();
    }
}
