package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

import org.junit.Before;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;

/**
 * Test class for testing configuring a simulation, TC-011 & ITC-012
 *
 * @author cbulloss
 * @author llaroussini
 */
public class SimulationConfigurationTests extends ETexasAfterTestResetTestBase {

    /**
     * User used in testing
     */
    private ETexasUser testuser;

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
        //User logged in and is on the simulations page (user will be left logged in after simulation creation)

        //Simulation created, no executions
        simulation = SimulationFactory.getTemplateSimulation(testuser, true);
        simulation.setUser(testuser);
        ETexasEntityManager.addEntities(testuser, simulation);
        ETexasSimulationUtils.createTemplateSimulation(simulation);

        //User logged in
        landing.loginAs(testuser);
    }

    //TODO - needs to be updated based on UI changes
    //	/**
    //	 * Test steps for TC-011
    //	 *
    //	 */
    //	@Test
    //	public void simulationConfigExternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Plan");
    //		//User is already logged in (preconditions)
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //		//Ensure correct page is displayed
    //		simPage.checkSimulationsHeaderText();
    //
    //		//Select an existing simulation with no executions (created in preconditions)
    //		simPage.selectSimCheckBox(simulation, true);
    //
    //		//Verify the configure button becomes enabled
    //		Assert.assertTrue(simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN));
    //
    //		//Hover over the Configure button and verify the following options display: Devices, Detectors, Options, Edit Sim Source
    //		simPage.clickEdit();
    //		simPage.checkEditOptions();
    //
    //		//Click Environment
    //		SimulationSettingsModal environmentForm = simPage.clickCompositeSettings();
    //
    //		//Check Tabs
    //		environmentForm.checkTabs();
    //
    //		//Click options tab
    //		SimOptionsForm optionsForm = environmentForm.clickOptionsTab();
    //
    //		//Verify the Configure Sim Options panel displays
    //		Assert.assertTrue("Configure Sim Options panel not displayed after clicking Options tab.", optionsForm.isConfigureOptionsPanelDisplayed());
    //
    //		//Verify '?' and 'x' icon display in upper right corner
    //		optionsForm.checkHeaderIcons();
    //
    //		//Verify the following sections display in the window: Simulation Center, Communications Model, Coordinate Conversion, RSE Coverage
    //		optionsForm.checkFormSections();
    //
    //		//Verify the following text boxes display in the Simulation center section: Latitude and Longitude
    //		optionsForm.checkSimCenterFields();
    //
    //		//Verify the following displays in the Communications Model section: Model Type dropdown
    //		Assert.assertTrue("The Model Type dropdown is not displayed in the options form.", optionsForm.isModelTypeMenuDisplayed());
    //
    //		//Verify the following displays in the Coordinate Conversion section: Coordinate System Dropdown
    //		Assert.assertTrue("The Coordinate System dropdown is not displayed in the options form.", optionsForm.isCoordinateSystemMenuDisplayed());
    //
    //		//Verify the following displays in the RSE coverage section: currently covered value
    //		Assert.assertTrue("The currently covered value is not displayed in the options form.", optionsForm.isCurrentlyCoveredValueDisplayed());
    //
    //		//Click the '?' icon
    //		ConfigureEnvironmentHelpForm helpForm = optionsForm.clickHelp();
    //
    //		//Verify that environment configuration help pop-up window displays with instructions
    //		Assert.assertTrue("Configure Environment help form header is not displayed as expected after clicking Help icon.", helpForm.isEnvironmentConfigHelpHeaderDisplayed());
    //		Assert.assertTrue("Configure Environment help content is not displayed as expected after clicking Help icon.", helpForm.isHelpContentDisplayed());
    //
    //		//verify an Ok button displays in the pop-up
    //		Assert.assertTrue("OK button is not displayed in Configure Environment Help window.", helpForm.isHelpOKBtnDisplayed());
    //
    //		//Click ok
    //		helpForm.clickHelpOKBtn();
    //
    //		//Verify help window closes
    //		Assert.assertFalse("Help window is still displayed after clicking OK.", helpForm.isEnvironmentConfigHelpHeaderDisplayed());
    //
    //		//Enter valid values in latitude and longitude boxes
    //		//Get initial values for comparison:
    //		//String initLat = optionsForm.getLatitudeText(); BUG 12410
    //		// initLon = optionsForm.getLongitudeText(); BUG 12410
    //		//First get valid lat and lon values:
    //		String format = "%.2f";
    //		String lat = String.format(format, RandomNumberGenerator.nextDouble(90));
    //		String lon = String.format(format, RandomNumberGenerator.nextDouble(90));
    //		//Now enter the values
    //		optionsForm.setLatitudeText(lat);
    //		optionsForm.setLongitudeText(lon);
    //
    //		//Select type from the Model Type dropdown
    //		//Verify initial selected option for comparison:
    //		Assert.assertTrue("'Idealized' model type not initally selected as expected.", optionsForm.isModelTypeSelected(ModelType.IDEALIZED));
    //		optionsForm.selectModelType(ModelType.NS3);
    //
    //		//Select Coordinate System
    //		//Verify initial selected option for comparison:
    //		Assert.assertTrue("'Geodetic 2D' coordinate system not initally selected as expected.", optionsForm.isCoordinateSysSelected(CoordinateSystem.GEODETIC_2D));
    //		optionsForm.selectCoordinateSystem(CoordinateSystem.CARTESIAN);
    //
    //		//click close
    //		optionsForm.clickCloseBtn();
    //
    //		//hover over the configure options and click enviroment
    //		simPage.clickEdit();
    //		environmentForm = simPage.clickCompositeSettings();
    //
    //		//Click the Options tab
    //		optionsForm = environmentForm.clickOptionsTab();
    //
    //		//Verify the Fields display the previously entered values.
    //		//Assert.assertEquals("Latitude text did not persist after clicking save.", lat, optionsForm.getLatitudeText()); BUG 12410
    //		//Assert.assertEquals("Longitude text did not persist after clicking save.", lon, optionsForm.getLongitudeText()); BUG 12410
    //		Assert.assertTrue("Model Type dropdown selection did not persist after closing window.", optionsForm.isModelTypeSelected(ModelType.NS3));
    //		Assert.assertTrue("Coordinate System dropdown selection not persist after closing window.", optionsForm.isCoordinateSysSelected(CoordinateSystem.CARTESIAN));
    //
    //		//click the Close button
    //		optionsForm.clickCloseBtn();
    //
    //		//Logout
    //		simPage.logout(testuser);
    //	}
    //TODO - needs to be updated based on UI changes

    //	/**
    //	 * Test steps for ITC-012
    //	 */
    //	@Test
    //	public void simulationConfigInternalTest() {
    //		//Set screenshot
    //		screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Plan");
    //		//User is already logged in (preconditions)
    //		SimulationsPage simPage = getPage(SimulationsPage.class);
    //
    //		//Select an existing simulation with no executions (created in preconditions)
    //		simPage.selectSimCheckBox(simulation, true);
    //		//Verify the configure button becomes enabled
    //		Assert.assertTrue(simPage.isSimBtnEnabled(SimBtns.CONFIGURE_BTN));
    //
    //		//Hover over the Configure button and verify expected options display
    //		simPage.clickEdit();
    //		simPage.checkEditOptions();
    //
    //		//Click Envrionment
    //		SimulationSettingsModal environmentPage = simPage.clickCompositeSettings();
    //
    //		//Click the Options tab
    //		SimOptionsForm optionsForm = environmentPage.clickOptionsTab();
    //
    //		//Delete the text from the Latitude and Longitude text boxes and click Close
    //		optionsForm.setLatitudeText("");
    //		optionsForm.setLongitudeText("");
    //		optionsForm.clickCloseBtnErrorExpected();
    //
    //		//Verify field required error is displayed associated with Latitude and Longitude text boxes
    //		optionsForm.checkLatitudeFieldRequiredErrorDisplayed();
    //		optionsForm.checkLongitudeFieldRequiredErrorDisplayed();
    //
    //		//Enter a valid value in the Longitude text box.
    //		optionsForm.setLongitudeText(Integer.toString(ETexasRandomDataGenerator.randomLongitudeValue()));
    //
    //		//Enter a value outside the range of -90 to 90 in the Latitude text box and click the Close button.
    //		optionsForm.setLatitudeText("91");
    //		optionsForm.clickCloseBtnErrorExpected();
    //
    //		//Verify error message displayed associated with Latitude text box
    //		Assert.assertTrue("Invalid latitude error tooltip could not be found after entering a latitude value outside the range of -90 and 90.", optionsForm.isInvalidLatitudeErrorDisplayed());
    //
    //		//Enter a non-numerical value in the Latitude text box
    //		optionsForm.copyPasteAlphabeticTextInLatitudeTextBox();
    //		ETexasCommonUtils.sleep(1000); //ZZZ - allows time for error to display (Save cannot be clicked here because doing so clears the alphabetic text from the field)
    //
    //		//Verify error message displayed associated with Latitude text box
    //		Assert.assertTrue("Non-numeric latitude error tooltip could not be found after entering an alphabetic value for latitude.", optionsForm.isNonNumericErrorDisplayed());
    //
    //		//Enter a numerical value between -90 and 90 in the Latitude text box.
    //		optionsForm.setLatitudeText(Integer.toString(ETexasRandomDataGenerator.randomLatitudeValue()));
    //
    //		//Enter a value outside the range of -180 to 180 in the Longitude text box and click the Close button.
    //		optionsForm.setLongitudeText("181");
    //		optionsForm.clickCloseBtnErrorExpected();
    //
    //		//Verify error message displayed associated with Longitude text box
    //		Assert.assertTrue("Invalid longitude error tooltip could not be found after entering a longitude value outside the range of -180 and 180.", optionsForm.isInvalidLongitudeErrorDisplayed());
    //
    //		//Enter a non-numerical value in the Longitude text box
    //		optionsForm.copyPasteAlphabeticTextInLongitudeTextBox();
    //
    //		//Verify error message displayed associated with Longitude text box
    //		Assert.assertTrue("Non-numeric longitude error tooltip could not be found after entering an alphabetic value for longitude.", optionsForm.isNonNumericErrorDisplayed());
    //
    //		//Enter a numerical value between -180 and 180 in the Longitude text box and click the Close button.
    //		optionsForm.setLongitudeText(Integer.toString(ETexasRandomDataGenerator.randomLongitudeValue()));
    //		optionsForm.clickCloseBtn();
    //		Assert.assertFalse("The Configure Sim Options form is still displayed after saving with valid values.", optionsForm.isConfigureOptionsPanelDisplayed());
    //
    //		//Logout
    //		simPage.logout(testuser);
    //
    //	}
}
