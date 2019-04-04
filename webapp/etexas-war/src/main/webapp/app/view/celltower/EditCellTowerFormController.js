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
 * @class ETexas.view.celltower.EditCellTowerFormController
 * @extends ETexas.view.model.ModelFormController
 * 
 * The {@link ETexas.view.celltower.EditCellTowerForm} controller.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.celltower.EditCellTowerFormController', {
    extend : 'ETexas.view.model.ModelFormController',
    alias : 'controller.editcelltowerform',

    /** @inheritdoc */
    init : function(view) {

        var selectedCellTower = this.getViewModel().get('selectedCellTower');

        view.getForm().setValues({
            provider : selectedCellTower.get('provider'),
            x : selectedCellTower.get('x'),
            y : selectedCellTower.get('y'),
            z : selectedCellTower.get('z')
        });

        this.callParent([ view ]);
    },

    /**
     * @method onHelpToolClicked
     * 
     * Handles the event that is generated when the user clicks the Edit Cell Tower Help tool.
     * 
     * @protected
     * @param {Ext.panel.Tool} tool The clicked tool.
     * @param {Ext.event.Event} e The click event.
     * @param {Ext.Component} owner The owner of the tool.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onHelpToolClicked : function(tool, e, owner, eOpts) {

        window.open(ETexas.util.UrlProvider.getHelpContentUrl() + '/Simulations/Composites/edit_cell_tower.htm');
    },

    /**
     * @method onUpdateButtonClicked
     * 
     * Handles the event that is generated when the user clicks the Update button.
     * 
     * @protected
     * @param {Ext.button.Button} button The clicked button.
     * @param {Ext.event.Event} e The click event.
     * @param {Object} eOpts The options passed to {@link Ext.util.Observable.addListener}.
     */
    onUpdateButtonClicked : function(button, e, eOpts) {

        var view = this.getView();

        if (view.isValid()) {

            var viewModel = this.getViewModel();
            var selectedComposite = viewModel.get('selectedComposite');
            var selectedCellTower = viewModel.get('selectedCellTower');

            view.mask('Updating Cell Tower...');
            view.submit({
                scope : this,
                method : 'PUT',
                url : ETexas.util.UrlProvider.getCellTowersUrl(selectedComposite, selectedCellTower),
                success : function(form, action) {

                    view.fireEvent('celltowerupdated', selectedCellTower.get('id'));
                    Ext.destroy(view);
                },
                failure : function(form, action) {

                    view.unmask();
                }
            });
        }
    }
});
