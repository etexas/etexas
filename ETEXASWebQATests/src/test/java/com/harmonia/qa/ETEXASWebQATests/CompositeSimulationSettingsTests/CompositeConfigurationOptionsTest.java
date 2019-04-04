package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasRandomDataGenerator;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeOptionsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeOptionsPartialPage.CompositeOptionsBtn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditOptionsForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditOptionsForm.GeoCalculatorOption;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.EditOptionsForm.ModelType;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;

/**
 * Test class for testing composite configuration options, TC-011 & ITC-012
 *
 * @author llaroussini
 */
public class CompositeConfigurationOptionsTest extends ETexasAfterTestResetTestBase {

    /**
     * User used in testing
     */
    private ETexasUser testuser;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation used in testing
     */
    private TemplateSimulation simulation;

    /**
     * Test setup & preconditions
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //User registered
        testuser = ETexasUserFactory.getUser(true);
        LandingPage landing = ETexasUserUtils.userRegistration(testuser);

        //Composite created, no executions
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testuser, simulation);
        ETexasSimulationUtils.createTemplateSimulation(simulation);

        //User logged in
        landing.loginAs(testuser);
    }

    /**
     * Test steps for TC-011: Composite Configuration Options
     */
    @Test
    public void compositeConfigOptionsExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Plan");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing composite with no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option and verify the Composite Settings modal is displayed.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();
        Assert.assertTrue("Composite Settings modal not displayed as expected after clicking Composite Settings option from Edit menu.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Click the Options tab.
        CompositeOptionsPartialPage optionsTab = compositeSettingsModal.clickOptionsTab();

        //Verify an Edit button is displayed at the top of the Options tab.
        optionsTab.checkEditBtnIsDisplayed();

        //Verify the Edit button is enabled.
        Assert.assertFalse("The Edit button is not enabbled as expected.", optionsTab.isBtnDisabled(CompositeOptionsBtn.EDIT));

        //Verify the following sections display in the Options tab: Coordinates and Communications.
        optionsTab.checkSections();

        //Verify the following values are displayed in the Coordinates section: Latitude (DD), Longitude (DD), and Geographic Calculator.
        optionsTab.checkCoordinatesDisplayFields();

        //Verify the Communications Model value is displayed in the Communications section.
        optionsTab.checkCommModelDisplayFieldIsDisplayed();

        //Automation only: get all displayed values (to be used later in test case)
        Double defaultLatitude = optionsTab.getDisplayedLatitude();
        Double defaultLongitude = optionsTab.getDisplayedLongitude();
        String defaultGeoCalculator = optionsTab.getDisplayedGeoCalculator();
        String defaultCommModel = optionsTab.getDisplayedCommModel();

        //Verify a Cellular Options button is displayed in the Communications section.
        optionsTab.checkCellOptionsBtnIsDisplayed();

        //Verify warning text is displayed below the Cellular Options button indicating cellular options can be configured at any time, but will only take effect when NS3 is the selected communications model.
        optionsTab.checkCommModelWarningIsDisplayed();

        //Verify a Close button is displayed at the bottom of the modal.
        Assert.assertTrue("Close button could not be found at bottom of Composite Settings modal with Options tab selected.", optionsTab.isCloseBtnDisplayed());

        //Click the Edit button.
        EditOptionsForm optionsForm = optionsTab.clickEdit();

        //Verify an Edit Options modal is displayed.
        optionsForm.checkEditOptionsHeader();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        optionsForm.checkEditOptionsHeaderIcons();

        //Click the ‘?’ icon.
        optionsForm.clickEditOptionsHelpIcon();

        //Verify that an Edit Options Help modal is displayed with instructions for editing the composite options and verify an OK button is displayed in the modal.
        optionsForm.checkEditOptionsHelpModal();

        //Click the OK button.
        optionsForm.clickEditOptionsHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Edit Options Help header still displayed after clicking OK.", optionsForm.isEditOptionsHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Edit Options modal: Latitude (DD) text box, Longitude (DD) text box, Geographic Calculator dropdown, and Communications Modal dropdown.
        optionsForm.checkFields();

        //Verify all fields are populated with the data previously displayed in Coordinates and Communications sections of the Options tab.
        optionsForm.checkValues(defaultLatitude, defaultLongitude, defaultGeoCalculator, defaultCommModel);

        //Verify the following buttons are displayed at the bottom of the Edit Options modal: Update, Reset, and Cancel.
        optionsForm.checkBtns();

        //Make a change to the Latitude (DD) value.
        String newLat = Integer.toString(RandomNumberGenerator.nextInteger((181) - 90));
        optionsForm.setLatitudeText(newLat);

        //Make a change to the Longitude (DD) value.
        String newLong = Integer.toString(RandomNumberGenerator.nextInteger((361) - 180));
        optionsForm.setLatitudeText(newLong);

        //Select a different Geographic Calculator value.
        GeoCalculatorOption newGeoCalc = GeoCalculatorOption.CARTESIAN;
        optionsForm.selectGeoCalculatorOption(newGeoCalc);

        //Select a different Communications Model value.
        ModelType newCommModel = ModelType.NS3;
        optionsForm.selectCommModel(newCommModel);

        //Click the Reset button.
        optionsForm.clickResetBtn();

        //Verify all values are reset to their original values.
        optionsForm.checkValues(defaultLatitude, defaultLongitude, defaultGeoCalculator, defaultCommModel);

        //Make a change to the Latitude (DD) value.
        optionsForm.setLatitudeText(newLat);

        //Make a change to the Longitude (DD) value.
        optionsForm.setLatitudeText(newLong);

        //Select a different Geographic Calculator value.
        optionsForm.selectGeoCalculatorOption(newGeoCalc);

        //Select a different Communications Model value.
        optionsForm.selectCommModel(newCommModel);

        //Click the Cancel button.
        optionsForm.clickCancelBtn();

        //Verify the Edit Options modal is no longer displayed.
        Assert.assertFalse("Edit Options modal header still displayed after clicking Cancel.", optionsForm.isEditOptionsHeaderDisplayed());

        //Verify the displayed values are unchanged in the Coordinates and Communications sections of the Options tab.
        optionsTab.checkValues(defaultLatitude, defaultLongitude, defaultGeoCalculator, defaultCommModel);

        //Click the Edit button.
        optionsTab.clickEdit();

        //Verify the Edit Options modal is displayed.
        optionsForm.checkEditOptionsHeader();

        //Make a change to the Latitude (DD) value.
        optionsForm.setLatitudeText(newLat);

        //Make a change to the Longitude (DD) value.
        optionsForm.setLongitudeText(newLong);

        //Select a different Geographic Calculator value.
        optionsForm.selectGeoCalculatorOption(newGeoCalc);

        //Select a different Communications Model value.
        optionsForm.selectCommModel(newCommModel);

        //Click the ‘x’ icon.
        optionsForm.clickEditOptionsCloseIcon();

        //Verify the Edit Options modal is no longer displayed.
        Assert.assertFalse("Edit Options modal header still displayed after clicking Close.", optionsForm.isEditOptionsHeaderDisplayed());

        //Verify the displayed values are unchanged in the Coordinates and Communications sections of the Options tab.
        optionsTab.checkValues(defaultLatitude, defaultLongitude, defaultGeoCalculator, defaultCommModel);

        //Click the Edit button.
        optionsTab.clickEdit();

        //Verify the Edit Options modal is displayed.
        optionsForm.checkEditOptionsHeader();

        //Make a change to the Latitude (DD) value.
        optionsForm.setLatitudeText(newLat);

        //Make a change to the Longitude (DD) value.
        optionsForm.setLongitudeText(newLong);

        //Select a different Geographic Calculator value.
        optionsForm.selectGeoCalculatorOption(newGeoCalc);

        //Select a different Communications Model value.
        optionsForm.selectCommModel(newCommModel);

        //Click the Update button.
        optionsForm.clickUpdateBtn(true);

        //Verify the Edit Options modal is no longer displayed.
        Assert.assertFalse("Edit Options modal header still displayed after clicking Update.", optionsForm.isEditOptionsHeaderDisplayed());

        //Verify all displayed values are updated in the Coordinates and Communications sections of the Options tab.
        optionsTab.checkValues(newLat, newLong, newGeoCalc.getLabel(), newCommModel.getLabel());

        //Click the Close button.
        optionsTab.clickCloseBtn();

        //Logout
        simPage.logout(testuser);
    }

    /**
     * Test steps for ITC-012: Composite Configuration Options
     */
    @Test
    public void compoisteConfigOptionsInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Plan");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing composite - with known settings
        simPage.selectComposite(composite, true);

        //Click Edit
        simPage.clickEdit();

        //Click Composite Settings
        CompositeSettingsModal settingsModal = simPage.clickCompositeSettings();

        //Click the Options tab
        CompositeOptionsPartialPage options = settingsModal.clickOptionsTab();

        //Click Edit
        EditOptionsForm optionsForm = options.clickEdit();

        //Delete the text from the Latitude and Longitude text boxes
        optionsForm.setLatitudeText("");
        optionsForm.setLongitudeText("");

        //Select a new Communications Model option
        ModelType newCommModel = ModelType.NS3;
        optionsForm.selectCommModel(newCommModel);
        Assert.assertTrue(newCommModel.getLabel() + " is not selected as expected.", optionsForm.isCommModelSelected(newCommModel));

        //Select a new Geographic Calculator option
        GeoCalculatorOption newGeoCalc = GeoCalculatorOption.CARTESIAN;
        optionsForm.selectGeoCalculatorOption(newGeoCalc);
        Assert.assertTrue(newGeoCalc.getLabel() + " is not selected as expected.", optionsForm.isGeoCalculatorOptionSelected(newGeoCalc));

        //Click Update
        optionsForm.clickUpdateBtn(false);

        //Verify field required error is displayed associated with Latitude and Longitude text boxes
        optionsForm.checkLatitudeFieldRequiredErrorDisplayed();
        optionsForm.checkLongitudeFieldRequiredErrorDisplayed();

        //Enter a non-numerical value in the Latitude text box
        optionsForm.setLatitudeText("--");

        //Verify an error icon associated with the Latitude text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric latitude error could not be found after entering an alphabetic value for latitude.", optionsForm.isLatitudeNonNumericErrorDisplayed());

        //Enter a value less than -90 in the Latitude text box
        optionsForm.setLatitudeText("-91");

        //Verify an error icon associated with the Latitude text box indicating latitude values must be in the range of -90 to 90 DD.
        Assert.assertTrue("Invalid latitude error could not be found after entering a latitude value less than -90.", optionsForm.isInvalidLatitudeErrorDisplayed());

        //Enter a value greater than 90 in the Latitude text box
        optionsForm.setLatitudeText("91");

        //Verify an error icon associated with the Latitude text box indicating latitude values must be in the range of -90 to 90 DD.
        Assert.assertTrue("Invalid latitude error could not be found after entering a latitude value greater than 90.", optionsForm.isInvalidLatitudeErrorDisplayed());

        //Enter a valid value in the Latitude text box.
        String newLat = Integer.toString(ETexasRandomDataGenerator.randomLatitudeValue());
        optionsForm.setLatitudeText(newLat);

        //Enter a non-numerical value in the Longitude text box
        optionsForm.setLongitudeText("--");

        //Verify an error icon associated with the Longitude text box indicating the value entered is not a valid number.
        Assert.assertTrue("Non-numeric longitude error could not be found after entering an alphabetic value for longitude.", optionsForm.isLongitudeNonNumericErrorDisplayed());

        //Enter a value less than -180 in the Longitude text box
        optionsForm.setLongitudeText("-181");

        //Verify an error icon associated with the Longitude text box indicates longitude values must be in the range of -180 to 180 DD.
        Assert.assertTrue("Invalid longitude error could not be found after entering a longitude value less than -180.", optionsForm.isInvalidLongitudeErrorDisplayed());

        //Enter a value greater than 180 in the Longitude text box
        optionsForm.setLongitudeText("181");

        //Verify an error icon associated with the Longitude text box indicates longitude values must be in the range of -180 to 180 DD.
        Assert.assertTrue("Invalid longitude error could not be found after entering a longitude value greater than 180.", optionsForm.isInvalidLongitudeErrorDisplayed());

        //Enter a valid value in the Longitude text box.
        String newLong = Integer.toString(ETexasRandomDataGenerator.randomLongitudeValue());
        optionsForm.setLongitudeText(newLong);

        //Click the Update button
        optionsForm.clickUpdateBtn(true);

        //Verify the values displayed in the Options match the updated values.
        options.checkValues(newLat, newLong, newGeoCalc.getLabel(), newCommModel.getLabel());

        //Click Close
        options.clickCloseBtn();

        //Logout
        simPage.logout(testuser);

    }
}
