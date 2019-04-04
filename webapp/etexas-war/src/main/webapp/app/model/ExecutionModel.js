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
 * @class ETexas.model.ExecutionModel
 * @extends Ext.data.Model
 * 
 * An execution of one or more traffic model simulations.
 * 
 * @author emyers
 */
Ext.define('ETexas.model.ExecutionModel', {
    extend : 'Ext.data.Model',

    fields : [ {
        name : 'id',
        type : 'int'
    }, {
        name : 'name',
        type : 'string'
    }, {
        name : 'status',
        type : 'string',
        convert : function(value) {

            if (value === 'IN_PROGRESS') {

                return 'In Progress';
            }

            if (value === 'COMPLETE') {

                return 'Complete';
            }

            if (value === 'ERROR') {

                return 'Error';
            }

            return value;
        }
    }, {
        name : 'maxSteps',
        type : 'int'
    }, {
        name : 'stepNumber',
        type : 'int'
    }, {
        name : 'stepSize',
        type : 'number'
    } ]
});
