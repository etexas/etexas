package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularOptionsConfiguration;
import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.CellularOptionsConfigurationFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeOptionsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditCellularOptionsModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;

/**
 * Test class for testing Composite Cellular Options, TC-078/ITC-059
 *
 * @author llaroussini
 */
public class CompositeCellularOptionsTest extends ETexasAfterTestResetTestBase {

    /**
     * User used in testing
     */
    private ETexasUser testuser;

    /**
     * Simulation used in testing
     */
    private TemplateSimulation simulation;

    /**
     * Composite used in testing
     */
    private CompositeSimulation composite;

    /**
     * Configuration of Cellular Options used in testing
     */
    private CellularOptionsConfiguration config;

    /**
     * Test setup & preconditions
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user, simulation and composite objects
        testuser = ETexasUserFactory.getUser(true);
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        simulation.setUser(testuser);
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testuser, simulation, composite);

        //User is registered and simulation/composite created
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);
        ETexasSimulationUtils.createTemplateSimulation(simulation);

        //Create configuration of advanced cellular options
        config = CellularOptionsConfigurationFactory.getAdvancedCellularConfiguration(true); //gets random configuration
        config.setSimulation(simulation);

        //User logged in
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-078
     */
    @Test
    public void compositeCellOptionsExternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Plan");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing composite with no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Verify the Composite Settings modal is displayed.
        Assert.assertTrue("Composite Setting modal not displayed as expected.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Click the Options tab.
        CompositeOptionsPartialPage optionsTab = compositeSettingsModal.clickOptionsTab();

        //Verify a Cellular Options button is displayed in the Communications section.
        optionsTab.checkCellOptionsBtnIsDisplayed();

        //Verify warning text is displayed below the Cellular Options button indicating cellular options can be configured at any time, but will only take effect when NS3 is the selected communications model.
        Assert.assertTrue("Warning text is not displayed in Cellular Options section as expected.", optionsTab.isCommModelWarningDisplayed());

        //Click the Cellular Options button.
        EditCellularOptionsModal cellOptionsModal = optionsTab.clickCellularOptions();

        //Verify an Edit Cellular Options modal is displayed.
        Assert.assertTrue("Edit Cellular Options modal is not displayed as expected after clicking Cellular Options button.", cellOptionsModal.isEditCellOptionsHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        cellOptionsModal.checkEditCellOptionsHeaderIcons();

        //Click the ‘?’ icon.
        cellOptionsModal.clickEditCellOptionsHelp();

        //Verify that an Edit Cellular Options Help modal is displayed with instructions for editing cellular options.
        cellOptionsModal.checkHelpModal();

        //Verify an OK button is displayed in the modal.
        Assert.assertTrue("OK button is not displayed in Edit Cellular Options Help modal as expected", cellOptionsModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        cellOptionsModal.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Edit Cellular Options Help modal is still displayed after clicking OK.", cellOptionsModal.isEditCellOptionsHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Edit Cellular Options modal: Uplink Bandwidth dropdown, Downlink Bandwidth dropdown, Uplink Carrier Frequency text box, Downlink Carrier Frequency text box, Cell Tower Noise (dB) text box, Cell Tower Power (dBm) text box, Cellular Device Noise (dB) text box, and Cellular Device Power (dBm) text box.
        cellOptionsModal.checkAllFields();

        //Verify all fields are populated with default data.
        cellOptionsModal.checkDefaultValuesDisplayed();

        //Verify the following buttons are displayed at the bottom of the Edit Options modal: Update, Reset, and Cancel.
        cellOptionsModal.checkAllBtns();

        //Update values in all fields
        cellOptionsModal.setAllFields(config);

        //Click the Reset button.
        cellOptionsModal.clickResetBtn();

        //Verify all values are reset to their original values.
        cellOptionsModal.checkDefaultValuesDisplayed();

        //Update values in all fields
        cellOptionsModal.setAllFields(config);

        //Click the Cancel button.
        cellOptionsModal.clickCancelBtn();

        //Verify the Edit Cellular Options modal is no longer displayed.
        Assert.assertFalse("Edit Cellular Options modal is still displayed after clicking Cancel.", cellOptionsModal.isEditCellOptionsHeaderDisplayed());

        //Click the Cellular Options button.
        optionsTab.clickCellularOptions();

        //Verify the Edit Options modal is displayed
        Assert.assertTrue("Edit Cellular Options modal is not displayed as expected after clicking Cellular Options button.", cellOptionsModal.isEditCellOptionsHeaderDisplayed());

        //Verify all fields are unchanged and display as their original default values.
        cellOptionsModal.checkDefaultValuesDisplayed();

        //Update values in all fields
        cellOptionsModal.setAllFields(config);

        //Click the ‘x’ icon.
        cellOptionsModal.clickCloseIcon();

        //Verify the Edit Cellular Options modal is no longer displayed.
        Assert.assertFalse("Edit Cellular Options modal is still displayed after clicking Close.", cellOptionsModal.isEditCellOptionsHeaderDisplayed());

        //Click the Cellular Options button.
        optionsTab.clickCellularOptions();

        //Verify the Edit Options modal is displayed
        Assert.assertTrue("Edit Cellular Options modal is not displayed as expected after clicking Cellular Options button.", cellOptionsModal.isEditCellOptionsHeaderDisplayed());

        //Verify all fields are unchanged and display as their original default values.
        cellOptionsModal.checkDefaultValuesDisplayed();

        //Update values in all fields
        cellOptionsModal.setAllFields(config);

        //Click the Update button.
        cellOptionsModal.clickUpdateBtn();

        //Verify the Edit Options modal is no longer displayed.
        Assert.assertFalse("Edit Cellular Options modal is still displayed after clicking Update.", cellOptionsModal.isEditCellOptionsHeaderDisplayed());

        //Click the Cellular Options button.
        optionsTab.clickCellularOptions();

        //Verify the Edit Options modal is displayed
        Assert.assertTrue("Edit Cellular Options modal is not displayed as expected after clicking Cellular Options button.", cellOptionsModal.isEditCellOptionsHeaderDisplayed());

        //Verify all fields are updated and display the values entered previously.
        cellOptionsModal.checkDisplayedValues(config);

        //Click the Cancel button.
        cellOptionsModal.clickCancelBtn();

        //Click the Close button.
        optionsTab.clickCloseBtn();

        //Log out
        simPage.logout(testuser);
    }

    /**
     * Test steps for ITC-059
     */
    @Test
    public void compositeCellOptionsInternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Plan");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing composite with no executions.
        simPage.selectComposite(composite, true);

        //Click Edit
        simPage.clickEdit();

        //Click Composite Settings option
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Click the Options tab.
        CompositeOptionsPartialPage optionsTab = compositeSettingsModal.clickOptionsTab();

        //Click Cellular Options button.
        EditCellularOptionsModal cellOptionsModal = optionsTab.clickCellularOptions();

        //Delete the pre-populated values from the following selectors: Uplink Carrier Frequency, Downlink Carrier Frequency, Cell Tower Noise, Cell Tower Power, Cellular Device Noise, and Cellular Device Power.
        cellOptionsModal.setUplinkCarrierFrequency("");
        cellOptionsModal.setDownlinkCarrierFrequency("");
        cellOptionsModal.setCellDeviceNoise("");
        cellOptionsModal.setCellDevicePower("");
        cellOptionsModal.setCellTowerNoise("");
        cellOptionsModal.setCellTowerPower("");

        //Click the Update button.
        cellOptionsModal.clickUpdateBtn();

        //Verify a field required error is displayed associated with the following fields: Uplink Carrier Frequency, Downlink Carrier Frequency, Cell Power, Cell Noise, Cell Tower Power, and Cell Tower Noise
        cellOptionsModal.checkRequiredFieldErrorAllFields();

        //Enter a non-numerical value in the Uplink Carrier Frequency selector (e.g., ‘---’).
        cellOptionsModal.setUplinkCarrierFrequency("---");

        //Verify an error is displayed associated with the Uplink Carrier Frequency selector indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid number error is not displayed with Uplink Carrier Frequency text box after entering a non-numeric value in the field.",
                cellOptionsModal.isUplinkCarrierFreqInvalidNumberErrorDisplayed());

        //Enter a value less than 18000 in the Uplink Carrier Frequency selector.
        cellOptionsModal.setUplinkCarrierFrequency(Integer.toString(RandomNumberGenerator.nextInteger(18000)));

        //Verify an error is displayed associated with the Uplink Carrier Frequency selector indicating the minimum acceptable value is 18000.
        Assert.assertTrue("Invalid range error is not displayed with Uplink Carrier Frequency text box after using a value less than the accepted minimum of 18000.",
                cellOptionsModal.isUplinkCarrierFreqInvalidRangeErrorDisplayed());

        //Enter a value greater than 24599 in the Uplink Carrier Frequency selector.
        cellOptionsModal.setUplinkCarrierFrequency(Integer.toString(cellOptionsModal.getRandomValue(24599, 100000)));

        //Verify an error is displayed associated with the Uplink Carrier Frequency selector indicating the maximum acceptable value is 24599.
        Assert.assertTrue("Invalid range error is not displayed with Uplink Carrier Frequency text box after using a value greater than the accepted maximum of 24599.",
                cellOptionsModal.isUplinkCarrierFreqInvalidRangeErrorDisplayed());

        //Enter a non-numerical value in the Downlink Carrier Frequency selector (e.g., ‘---’).
        cellOptionsModal.setDownlinkCarrierFrequency("---");

        //Verify an error is displayed associated with the Downlink Carrier Frequency selector indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid number error is not displayed with Downlink Carrier Frequency text box after entering a non-numeric value in the field.",
                cellOptionsModal.isDownlinkCarrierFreqInvalidNumberErrorDisplayed());

        //Enter a value greater than 6599 in the Downlink Carrier Frequency selector.
        cellOptionsModal.setDownlinkCarrierFrequency(Integer.toString(cellOptionsModal.getRandomValue(6599, 100000)));

        //Verify an error is displayed associated with the Downlink Carrier Frequency selector indicating the maximum acceptable value is 6599.
        Assert.assertTrue("Invalid range error is not displayed with Downlink Carrier Frequency text box after using a value greater than the accepted maximum of 6599.",
                cellOptionsModal.isDownlinkCarrierFreqInvalidRangeErrorDisplayed());

        //Enter a non-numerical value in the Cell Tower Noise selector (e.g., ‘---’).
        cellOptionsModal.setCellTowerNoise("---");

        //Verify an error is displayed associated with the Cell Tower Noise text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid number error is not displayed with Cell Tower Noise text box after entering a non-numeric value in the field.",
                cellOptionsModal.isCellTowerNoiseInvalidNumberErrorDisplayed());

        //Enter a value greater than 194 in the Cell Tower Noise Figure text box.
        cellOptionsModal.setCellTowerNoise(Integer.toString(cellOptionsModal.getRandomValue(195, 1000)));

        //Verify an error is displayed associated with the Cell Tower Noise Figure text box indicating the maximum acceptable value is 194.
        Assert.assertTrue("Invalid range error is not displayed with Cell Tower Noise text box after using a value greater than the accepted maximum of 194.",
                cellOptionsModal.isCellTowerNoiseInvalidRangeErrorDisplayed());

        //Enter a non-numeric value in the Cell Tower Power selector (e.g. ‘---‘)
        cellOptionsModal.setCellTowerPower("---");

        //Verify an error is displayed associated with the Cell Tower Power selector indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid number error is not displayed with Cell Tower Tx Power text box after entering a non-numeric value in the field.",
                cellOptionsModal.isCellTowerPowerInvalidNumberErrorDisplayed());

        //Enter a value less than -200 in the Cell Tower Power selector.
        cellOptionsModal.setCellTowerPower(Integer.toString(cellOptionsModal.getRandomValue(-1000, -200)));

        //Verify an error is displayed associated with the Cell Tower Power selector indicating the minimum acceptable value is -200.
        Assert.assertTrue("Invalid range error is not displayed with Cell Tower Power text box after using a value less than the accepted minimum of -200.",
                cellOptionsModal.isCellTowerPowerInvalidRangeErrorDisplayed());

        //Enter a value greater than 200 in the Cell Tower Power selector.
        cellOptionsModal.setCellTowerPower(Integer.toString(cellOptionsModal.getRandomValue(200, 1000)));

        //Verify an error is displayed associated with the Cell Tower Power selector indicating the maximum acceptable value is 200.
        Assert.assertTrue("Invalid range error is not displayed with Cell Tower Power text box after using a value greater than the accepted maximum of 200.",
                cellOptionsModal.isCellTowerPowerInvalidRangeErrorDisplayed());

        //Enter a non-numeric value in the Cell Noise selector.
        cellOptionsModal.setCellDeviceNoise("---");

        //Verify an error is displayed associated with the Cell Noise text box indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid number error is not displayed with Cell Noise Figure text box after entering a non-numeric value in the field.",
                cellOptionsModal.isCellNoiseInvalidNumberErrorDisplayed());

        //Enter a value greater than 194 in the Cell Noise text box.
        cellOptionsModal.setCellDeviceNoise(Integer.toString(cellOptionsModal.getRandomValue(195, 1000)));

        //Verify an error is displayed associated with the Cell Noise text box indicating the maximum acceptable value is 194.
        Assert.assertTrue("Invalid range error is not displayed with Cell Noise Figure text box after using a value greater than the accepted maximum of 194.",
                cellOptionsModal.isCellNoiseInvalidRangeErrorDisplayed());

        //Enter a non-numeric value in the Cell Power selector (e.g. ‘---‘)
        cellOptionsModal.setCellDevicePower("---");

        //Verify an error is displayed associated with the Cell Power selector indicating the value entered is not a valid number.
        Assert.assertTrue("Invalid number error is not displayed with Cell Tx Power text box after entering a non-numeric value in the field.",
                cellOptionsModal.isCellPowerInvalidNumberErrorDisplayed());

        //Enter a value less than -200 in the Cell Power selector.
        cellOptionsModal.setCellDevicePower(Integer.toString(cellOptionsModal.getRandomValue(-1000, -200)));

        //Verify an error is displayed associated with the Cell Power selector indicating the minimum acceptable value is -200.
        Assert.assertTrue("Invalid range error is not displayed with Cell Tx Power text box after using a value less than the accepted minimum of -200.",
                cellOptionsModal.isCellPowerInvalidRangeErrorDisplayed());

        //Enter a value greater than 200 in the Cell Power selector.
        cellOptionsModal.setCellDevicePower(Integer.toString(cellOptionsModal.getRandomValue(200, 1000)));

        //Verify an error is displayed associated with the Cell Power selector indicating the maximum acceptable value is 200.
        Assert.assertTrue("Invalid range error is not displayed with Cell Tx Power text box after using a value greater than the accepted maximum of 200.",
                cellOptionsModal.isCellPowerInvalidRangeErrorDisplayed());

        //Enter valid values in all fields
        cellOptionsModal.setAllFields(config);

        //Click the Update button.
        cellOptionsModal.clickUpdateBtn();

        //Click the Cellular Options button again and verify changed values were saved and are persisted in all fields.
        cellOptionsModal = optionsTab.clickCellularOptions();
        cellOptionsModal.checkDisplayedValues(config);

        //Click the Cancel button
        cellOptionsModal.clickCancelBtn();

        //Click the Close button in the Composite Settings modal.
        compositeSettingsModal.clickCloseBtn();

        //Logout
        simPage.logout(testuser);

    }
}
