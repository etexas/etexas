package com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CreateSimulationFromTemplateModal;

/**
 * Test class which executes steps for the Create a New Simulation From a
 * Template test, TC-004 and ITC-003
 *
 * @author llaroussini
 * @author rsmith
 */
public class CreateANewTemplateSimulationInACompositeTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used for testing
     */
    private TemplateSimulation testSim;

    /**
     * Additional simulation object used for testing
     */
    private TemplateSimulation testSim2;

    /**
     * Existing simulation object used to meet preconditions of test case
     */
    private TemplateSimulation existingSim;

    /**
     * Composite object used to meet preconditions of test case
     */
    private CompositeSimulation composite;

    /**
     * Additional exisitng simulation object used to meet preconditions of test
     * case
     */
    private TemplateSimulation existingSim2;

    /**
     * Additional composite object used to meet preconditions of test case
     */
    private CompositeSimulation composite2;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user and simulation/composites for testing
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        existingSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = existingSim.getComposite();
        existingSim2 = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite2 = existingSim2.getComposite();
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        testSim.setComposite(composite);
        testSim2 = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        testSim.setComposite(composite2);
        ETexasEntityManager.addEntities(testUser, testSim, testSim2);

        //Register user
        LandingPage landingPage = ETexasUserUtils.userRegistration(testUser);

        //Create existing simulations
        ETexasSimulationUtils.createTemplateSimulation(existingSim);
        ETexasSimulationUtils.createTemplateSimulation(existingSim2);

        //Login
        landingPage.loginAs(testUser);
    }

    /**
     * Test steps for TC-004
     */
    @Test
    public void newSimFromTemplateExternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite name from the Simulations list.
        simPage.selectComposite(composite, true);

        //Verify that the Create, Edit, and Delete buttons are enabled.
        simPage.checkEnabledSimulationBtns();

        //Click the Create button.
        simPage.clickCreate();

        //Verify Template and Upload options are displayed.
        simPage.checkCreateOptions();

        //Click the Template option.
        CreateSimulationFromTemplateModal templateSimForm = simPage.clickTemplate();

        //Verify the Create Simulation from Template modal is displayed.
        templateSimForm.checkTemplateSimHeader();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        templateSimForm.checkTemplateSimHeaderIcons();

        //Click the ‘?’ icon.
        templateSimForm.clickTemplateSimHelp();

        //Verify a Create Simulation from Template Help modal is displayed with instructions for creating a simulation from a template.
        templateSimForm.checkHelpModal();

        //Verify an OK button is displayed and click the OK button.
        Assert.assertTrue("OK button is not displayed in Create Simulation from Template Help modal.", templateSimForm.isHelpOKBtnDisplayed());
        templateSimForm.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("The Create Simulation from Template Help header is still displayed after clicking OK.", templateSimForm.isCreateSimTemplateHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Create Simulation from Template modal: Composite text box, Select button, Name text box, Template dropdown and Description label.
        templateSimForm.checkFields();

        //Verify the following buttons are displayed at the bottom of the Create Simulation from Template modal: Create, Reset, and Cancel.
        templateSimForm.checkBtns();

        //Verify the Composite text box is pre-filled with the selected composite name.
        String compositeName = composite.getName();
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, templateSimForm.getCompositeName());

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim);

        //Click the Template dropdown and verify that a list of templates is displayed.
        templateSimForm.checkTemplateList();

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim);

        //Verify a description of the template is displayed next to the Description label.
        Assert.assertEquals("Displayed template description does not match expected description.", testSim.getTemplate().getDescription(), templateSimForm.getTemplateDescription());

        //Click the Reset button and verify all values are returned to their default state.
        templateSimForm.clickReset();
        templateSimForm.checkResetFields(composite.getName());

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, templateSimForm.getCompositeName());

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim);

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim);

        //Click the Cancel button.
        simPage = templateSimForm.clickCancel();

        //Verify the Create Simulation from Template modal is no longer displayed.
        Assert.assertFalse("The Create Simulation from Template header is still displayed after clicking the Cancel button.", templateSimForm.isCreateSimTemplateHeaderDisplayed());

        //Verify the simulation is not listed under the composite name.
        simPage.expandComposite(composite, true);
        Assert.assertFalse("New simulation is displayed under composite after clicking Cancel button.", simPage.isSimDisplayed(testSim));

        //Click the Create button then click the Template option.
        simPage.clickCreate();
        templateSimForm = simPage.clickTemplate();

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, templateSimForm.getCompositeName());

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim);

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim);

        //Click the ‘x’ icon.
        simPage = templateSimForm.clickCloseIcon();

        //Verify the Create Simulation from Template modal is no longer displayed.
        Assert.assertFalse("The Create Simulation from Template header is still displayed after clicking the Close icon.", templateSimForm.isCreateSimTemplateHeaderDisplayed());

        //Verify the simulation is not listed under the composite name.
        Assert.assertFalse("New simulation is displayed under composite after clicking Close icon.", simPage.isSimDisplayed(testSim)); //note: the composite only needs to be expanded once, after doing so it remains expanded

        //Click the Create button then click the Template option.
        simPage.clickCreate();
        templateSimForm = simPage.clickTemplate();

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName, templateSimForm.getCompositeName());

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim);

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim);

        //Click the Create button.
        simPage = templateSimForm.clickCreate(true);

        //Verify the Create Simulation from Template modal is no longer displayed.
        Assert.assertFalse("The Create Simulation from Template header is still displayed after clicking the Create button.", templateSimForm.isCreateSimTemplateHeaderDisplayed());

        //Verify the new simulation is listed under the selected composite.
        Assert.assertTrue("New simulation is not displayed after successful creation.", simPage.isSimDisplayed(testSim));

        //Select any other composite.
        simPage.selectComposite(composite2, true);

        //Click the Create button then click the Template option.
        simPage.clickCreate();
        templateSimForm = simPage.clickTemplate();
        templateSimForm.checkBtns();

        //Verify the Composite text box is pre-filled with the selected composite name.
        String compositeName2 = composite2.getName();
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName2, templateSimForm.getCompositeName());

        //Click the Select button and verify a list of all the user’s composites is displayed.
        templateSimForm.checkCompositeList();

        //Select a different composite name.
        templateSimForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim2);

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim2);

        //Click the Reset button and verify all values are returned to their default state.
        templateSimForm.clickReset();
        templateSimForm.checkResetFields(composite2.getName());

        //Click the Select button and select a different composite name.
        templateSimForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim2);

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim2);

        //Click the Cancel button.
        simPage = templateSimForm.clickCancel();

        //Verify the Create Simulation from Template modal is no longer displayed.
        Assert.assertFalse("The Create Simulation from Template header is still displayed after clicking the Cancel button.", templateSimForm.isCreateSimTemplateHeaderDisplayed());

        //Verify the simulation is not listed under the composite name.
        Assert.assertFalse("New simulation is displayed under composite after clicking Cancel button.", simPage.isSimDisplayed(testSim2)); //note: the composite only needs to be expanded once, after doing so it remains expanded

        //Click the Create button then click the Template option.
        simPage.clickCreate();
        templateSimForm = simPage.clickTemplate();

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName2, templateSimForm.getCompositeName());

        //Click the Select button and select a different composite name.
        templateSimForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim2);

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim2);

        //Click the ‘x’ icon.
        simPage = templateSimForm.clickCloseIcon();

        //Verify the Create Simulation from Template modal is no longer displayed.
        Assert.assertFalse("The Create Simulation from Template header is still displayed after clicking the Close icon.", templateSimForm.isCreateSimTemplateHeaderDisplayed());

        //Verify the simulation is not listed under the composite name.
        Assert.assertFalse("New simulation is displayed under composite after clicking Close icon.", simPage.isSimDisplayed(testSim2)); //note: the composite only needs to be expanded once, after doing so it remains expanded

        //Click the Create button then click the Template option.
        simPage.clickCreate();
        templateSimForm = simPage.clickTemplate();

        //Verify the Composite text box is pre-filled with the selected composite name.
        Assert.assertEquals("Displayed composite name does not match expected name.", compositeName2, templateSimForm.getCompositeName());

        //Click the Select button and select a different composite name.
        templateSimForm.selectComposite(composite);

        //Enter a name for the simulation into the Name text box.
        templateSimForm.setSimName(testSim2);

        //Select a template from the Template dropdown.
        templateSimForm.selectTemplate(testSim2);

        //Click the Create button.
        templateSimForm.clickCreate(true);

        //Verify the Create Simulation from Template modal closes.
        Assert.assertFalse("The Create Simulation from Template header is still displayed after clicking the Create button.", templateSimForm.isCreateSimTemplateHeaderDisplayed());

        //Verify the new simulation is listed under the selected composite.
        Assert.assertTrue("New simulation is not displayed after successful creation.", simPage.isSimDisplayed(testSim2));

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-003
     */
    @Test
    public void newSimFromTemplateInternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");
        //Verify simulations page is displayed
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Hover over New and click Template
        simPage.clickCreate();
        CreateSimulationFromTemplateModal templateSimForm = simPage.clickTemplate();

        //With all fields blank, click Create
        templateSimForm.clickCreate(false);

        //Verify 'field required' errors display associated with name and template fields
        templateSimForm.checkSimNameFieldRequiredErrorDisplayed();
        templateSimForm.checkTemplateFieldRequiredErrorDisplayed();

        //Verify text: 'No template selected' is displayed
        templateSimForm.checkNoTemplateSelectedText();

        //Enter a few spaces in the name field, select a template and click Create
        templateSimForm.setSimName("   ");
        templateSimForm.selectTemplate(testSim);
        templateSimForm.clickCreate(false);

        //Verify field required error displays associated with name field
        templateSimForm.checkSimNameFieldRequiredErrorDisplayed();

        //Enter a value in the name field containing special characters and click Create
        templateSimForm.setSimName("Te$t!");
        templateSimForm.clickCreate(false);

        //Verify error displays associated with name field
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with special characters.", templateSimForm.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the name field with consecutive spaces between characters and click Create
        templateSimForm.setSimName("Te  st");
        templateSimForm.clickCreate(false);

        //Verify error displays associated with name field
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with consecutive spaces between characters.",
                templateSimForm.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the name field with a leading space and click Create
        templateSimForm.setSimName(" Test");
        templateSimForm.clickCreate(false);

        //Verify error displays associated with name field
        templateSimForm.checkSimNameWhitespaceErrorDisplayed();

        //Enter a value in the name field with a trailing space and click Create
        templateSimForm.setSimName("Test ");
        templateSimForm.clickCreate(false);

        //Verify error displays associated with name field
        templateSimForm.checkSimNameWhitespaceErrorDisplayed();

        //Enter a value in the name field with a leading dash (-) and click Create
        templateSimForm.setSimName("-Test");
        templateSimForm.clickCreate(false);

        //Verify error displays associated with name field
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with a leading dash.", templateSimForm.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the name field with a trailing dash and click Create
        templateSimForm.setSimName("Test-");
        templateSimForm.clickCreate(false);

        //Verify error displays associated with name field
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a simulation name with a trailing dash.", templateSimForm.isSimulationNameInvalidErrorDisplayed());

        //Enter valid values and click Create
        templateSimForm.setAllFields(testSim);
        templateSimForm.checkAllDisplayedFields(testSim);
        templateSimForm.clickCreate(true);

        //Verify new simulation is displayed in simulation list
        Assert.assertTrue("New simulation is not displayed.", simPage.isSimDisplayed(testSim));
        simPage.checkSimulationDetails(testSim);

        //Verify the newly created composite is selected, if not, select the newly created composite.
        if (simPage.isCompositeDisplayed(composite2) == false) {
            simPage.selectComposite(composite, true);
        }

        //Click the Create Button
        simPage.clickCreate();

        //Click the Template link
        simPage.clickTemplate();

        //Select the same Composite used in previous steps
        templateSimForm.selectComposite(composite2);

        //Enter the same name used in previous steps in the Name text box.
        templateSimForm.setSimName(testSim);

        //Select a valid template from dropdown
        templateSimForm.selectTemplate(testSim);

        //Click the Create button
        templateSimForm.clickCreate(false);

        //Verify an Error is displayed associated with the Name text box indicating the simulation name must be unique
        Assert.assertTrue("Invalid Sim Name error tooltip could not be found after entering a duplicate simulation name.", templateSimForm.isDuplicateSimulationNameErrorDisplayed(testSim.getName()));

        //Click the Cancel button.
        templateSimForm.clickCancel();

        //Logout
        simPage.logout(testUser);
    }
}
