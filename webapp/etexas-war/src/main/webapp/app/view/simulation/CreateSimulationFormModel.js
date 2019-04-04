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
 * @class ETexas.view.simulation.CreateSimulationFormModel
 * @extends Ext.app.ViewModel
 * 
 * The {@link ETexas.view.simulation.CreateSimulationForm} model.
 * 
 * @author emyers
 */
Ext.define('ETexas.view.simulation.CreateSimulationFormModel', {
    extend : 'Ext.app.ViewModel',
    alias : 'viewmodel.createsimulationform',

    stores : {

        /** @property {Ext.data.Store} templateStore The simulation template data. */
        templateStore : {

            fields : [ {
                name : 'name',
                type : 'string'
            }, {
                name : 'description',
                type : 'string'
            } ],

            data : [ {
                name : 'exam_01',
                description : '4X4  LIGHT TRAFFIC'
            }, {
                name : 'exam_02',
                description : '4X4  LIGHT TRAFFIC'
            }, {
                name : 'exam_03',
                description : '4X4  LIGHT TRAFFIC'
            }, {
                name : 'exam_04',
                description : '4X4  LIGHT TRAFFIC'
            }, {
                name : 'exam_05',
                description : '4X4  LIGHT TRAFFIC'
            }, {
                name : 'exam_06',
                description : '4X4  LIGHT TRAFFIC'
            }, {
                name : 'exam_07',
                description : '4X4  LIGHT TRAFFIC'
            }, {
                name : 'exam_08',
                description : '5X4  HEAVY TRAFFIC'
            }, {
                name : 'exam_09',
                description : '5X4  HEAVY TRAFFIC'
            }, {
                name : 'exam_10',
                description : '5X4  HEAVY TRAFFIC'
            }, {
                name : 'exam_11',
                description : 'COMPACT DIAMOND'
            }, {
                name : 'exam_12',
                description : 'COMPACT DIAMOND'
            }, {
                name : 'exam_13',
                description : 'EX1  STANDARD DIAMOND'
            }, {
                name : 'exam_14',
                description : 'EX1  STANDARD DIAMOND'
            }, {
                name : 'exam_15',
                description : 'EX1  STANDARD DIAMOND'
            }, {
                name : 'exam_16',
                description : 'EX1  STANDARD DIAMOND'
            }, {
                name : 'exam_17',
                description : 'EX1  STANDARD DIAMOND'
            }, {
                name : 'exam_18',
                description : 'STANDARD DIAMOND FREE U-TURNS'
            }, {
                name : 'exam_19',
                description : '5X5  HEAVY TRAFFIC'
            }, {
                name : 'exam_20',
                description : '5X5  HEAVY TRAFFIC'
            } ]
        }
    }
});
