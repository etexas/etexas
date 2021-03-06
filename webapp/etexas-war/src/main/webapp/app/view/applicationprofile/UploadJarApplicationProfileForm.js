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
 * @class ETexas.view.applicationprofile.UploadJarApplicationProfileForm
 * @extends ETexas.view.applicationprofile.JarApplicationProfileForm
 * 
 * A form panel to upload new JAR {@link ETexas.model.ApplicationProfileModel}(s).
 * 
 * @author emyers
 */
Ext.define('ETexas.view.applicationprofile.UploadJarApplicationProfileForm', {
    extend : 'ETexas.view.applicationprofile.JarApplicationProfileForm',
    xtype : 'uploadjarapplicationprofileform',

    requires : [ 'ETexas.view.applicationprofile.UploadJarApplicationProfileFormController' ],

    controller : 'uploadjarapplicationprofileform',

    title : 'Upload JAR Applications',

    /** @inheritdoc */
    buildButtons : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            reference : 'uploadButton',
            id : Ext.id(null, 'upload-button-'),
            handler : 'onUploadButtonClicked',
            text : 'Upload'
        } ]);
    },

    /** @inheritdoc */
    buildItems : function() {

        var fileMsg = 'A valid applications (.jar) file is required.';

        return Ext.Array.push(this.callParent(), {
            xtype : 'filefield',
            reference : 'fileField',
            id : Ext.id(null, 'file-field-'),
            fieldLabel : 'Upload',
            minWidth : 400,
            regex : /^.*\.(jar|JAR)$/,
            regexText : fileMsg,
            allowBlank : false,
            blankText : fileMsg
        });
    },

    /** @interitdoc */
    buildTools : function() {

        return Ext.Array.insert(this.callParent(), 0, [ {
            type : 'help',
            reference : 'helpTool',
            id : Ext.id(null, 'help-tool-'),
            callback : 'onHelpToolClicked',
            tooltip : 'Upload JAR Applications Help'
        } ]);
    }
});
