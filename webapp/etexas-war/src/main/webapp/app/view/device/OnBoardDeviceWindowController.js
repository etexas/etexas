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
/**
 * @class ETexas.view.device.OnBoardDeviceWindowController
 * @extends ETexas.view.window.BasicWindowController
 * 
 * The {@link ETexas.view.device.OnBoardDeviceWindow} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.device.OnBoardDeviceWindowController', {
    extend : 'ETexas.view.window.BasicWindowController',
    alias : 'controller.onboarddevicewindow',

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the On Board Devices Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Executions/vehicle_info.htm');
    },

    /**
     * @method showApplications
     * 
     * Shows the dialog to display the applications for a specified on board device.
     * 
     * @protected
     * @param {ETexas.model.OnBoardDeviceModel} device The specified on board device.
     */
    showApplications : function(device) {

        var window = Ext.create('ETexas.view.application.DeployedApplicationWindow', {
            id : Ext.id(null, 'deployed-application-window-')
        });

        var composite = this.getViewModel().get('selectedComposite');
        var deviceProfileModel = Ext.create('ETexas.model.DeviceProfileModel');
        deviceProfileModel.set('id', device.get('deviceRuleId'));
        window.getViewModel().set('applicationsUrl', ETexas.util.UrlProvider.getDeviceProfileApplicationsUrl(composite, deviceProfileModel));
        window.show();
    },

    /**
     * @method showMessages
     * 
     * Shows the dialog to display the messages for a specified on board device.
     * 
     * @protected
     * @param {ETexas.model.OnBoardDeviceModel} device The specified on board device.
     */
    showMessages : function(device) {

        var window = Ext.create('ETexas.view.message.MessageWindow', {
            id : Ext.id(null, 'message-window-'),
            viewModel : {
                parent : this.getViewModel()
            }
        });

        window.getViewModel().set('deviceMac', device.get('deviceMac'));
        window.show();
    }
});
