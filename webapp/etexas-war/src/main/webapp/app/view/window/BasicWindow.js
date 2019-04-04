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
 * @class ETexas.view.window.BasicWindow
 * @extends Ext.window.Window
 * 
 * A basic modal window with close functionality.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.window.BasicWindow', {
    extend : 'Ext.window.Window',
    xtype : 'basicwindow',

    requires : [ 'ETexas.view.window.BasicWindowController' ],

    controller : 'basicwindow',

    closable : false,
    modal : true,

    /** @inheritdoc */
    initComponent : function() {

        this.buttons = this.buildButtons();
        this.tools = this.buildTools();
        this.callParent();
    },

    /**
     * @method buildButtons
     * 
     * Builds the buttons for this window.
     * 
     * @template
     * @protected
     * @return {Object[]} The buttons for this window.
     */
    buildButtons : function() {

        return [ {
            reference : 'closeButton',
            id : Ext.id(null, 'close-button-'),
            handler : 'onCloseButtonClicked',
            text : 'Close'
        } ];
    },

    /**
     * @method buildTools
     * 
     * Builds the tools for this window.
     * 
     * @template
     * @protected
     * @return {Ext.panel.Tool[]} The tools for this window.
     */
    buildTools : function() {

        return [ {
            type : 'close',
            reference : 'closeTool',
            id : Ext.id(null, 'close-tool-'),
            callback : 'onCloseToolClicked',
            tooltip : 'Close'
        } ];
    }
});
