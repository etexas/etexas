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
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.SimBtns;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CreateSimulationFromTemplateModal;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Create A New Composite with Template
 * Simulation Test, TC-082/ITC-072
 *
 * @author llaroussini
 */
public class CreateANewCompositeWithTemplateSimulationTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used for testing
     */
    private TemplateSimulation simulation;

    /**
     * Composite object used for testing
     */
    private CompositeSimulation composite;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user and simulation/composites for testing
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testUser, simulation, composite);

        //Register user
        LandingPage landingPage = ETexasUserUtils.userRegistration(testUser);

        //Login
        landingPage.loginAs(testUser);
    }

    /**
     * Test steps for TC-082
     */
    @Test
    public void newCompositeFromTemplateExternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Verify the Create button is enabled.
        Assert.assertTrue("The create button is not enabled upon initial login.", simPage.isBtnEnabled(SimBtns.CREATE_BTN));

        //Click the Create button.
        simPage.clickCreate();

        //Verify Template and Upload options are displayed.
        simPage.checkCreateOptions();

        //Click the Template option.
        CreateSimulationFromTemplateModal templateForm = simPage.clickTemplate();

        //Verify a Create Simulation from Template modal is displayed.
        templateForm.checkTemplateSimHeader();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        templateForm.checkTemplateSimHeaderIcons();

        //Click the ‘?’ icon.
        templateForm.clickTemplateSimHelp();

        //Verify a Create Simulation from Template Help modal is displayed with instructions for creating a simulation from a template.
        templateForm.checkHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button not displayed in Create Simulation from Template Help modal.", templateForm.isHelpOKBtnDisplayed());

        //Click the OK button.
        templateForm.clickHelpOKBtn();

        //Verify the Help modal closes.
        Assert.assertFalse("Create Simulation from Template Help modal is still displayed after clicking OK.", templateForm.isCreateSimTemplateHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Create Simulation from Template modal: Composite text box, Select button, Name text box, Template dropdown and Description label.
        templateForm.checkFields();

        //Verify the following buttons are displayed at the bottom of the Create Simulation from Template modal: Create, Reset, and Cancel.
        templateForm.checkBtns();

        //Enter a name for the composite in the Composite text box.
        templateForm.setCompositeName(composite);

        //Enter a name for the simulation into the Name text box.
        templateForm.setSimName(simulation);

        //Click the Template dropdown and verify a list of templates is displayed.
        templateForm.checkTemplateList();

        //Select a template from the list.
        templateForm.selectTemplate(simulation);

        //Verify a description of the template is displayed next to the Description label.
        Assert.assertEquals("Displayed template description does not match expected description.", simulation.getTemplate().getDescription(), templateForm.getTemplateDescription());

        //Click the Reset button.
        templateForm.clickReset();

        //Verify all values are returned to their default state.
        templateForm.checkResetFields("");

        //Enter a name for the composite in the Composite text box.
        templateForm.setCompositeName(composite);

        //Enter a name for the simulation into the Name text box.
        templateForm.setSimName(simulation);

        //Select a template from the Template dropdown.
        templateForm.selectTemplate(simulation);

        //Click the Cancel button.
        templateForm.clickCancel();

        //Verify the Create Simulation from Template modal is no longer displayed.
        Assert.assertFalse("Create Simulation from Template modal is still displayed despite clicking Cancel.", templateForm.isCreateSimTemplateHeaderDisplayed());

        //Verify neither the composite nor individual simulation are displayed in the list of simulations.
        Assert.assertFalse("New composite is displayed despite cancelling creation of composite.", simPage.isCompositeDisplayed(composite));
        Assert.assertFalse("New simulation is displayed despite cancelling creation of composite.", simPage.isSimDisplayed(simulation));

        //Click the Create button and click the Template option.
        simPage.clickCreate();
        simPage.clickTemplate();

        //Verify default values are displayed in all fields.
        templateForm.checkResetFields("");

        //Enter a name for the composite in the Composite text box.
        templateForm.setCompositeName(composite);

        //Enter a name for the simulation into the Name text box.
        templateForm.setSimName(simulation);

        //Select a template from the Template dropdown.
        templateForm.selectTemplate(simulation);

        //Click the ‘x’ icon.
        templateForm.clickCloseIcon();

        //Verify the Create Simulation from Template modal is no longer displayed.
        Assert.assertFalse("Create Simulation from Template modal is still displayed despite clicking the Close icon.", templateForm.isCreateSimTemplateHeaderDisplayed());

        //Verify neither the composite nor individual simulation are displayed in the list of simulations.
        Assert.assertFalse("New composite is displayed despite closing the Create Simulation from Template modal.", simPage.isCompositeDisplayed(composite));
        Assert.assertFalse("New simulation is displayed despite closing the Create Simulation from Template modal.", simPage.isSimDisplayed(simulation));

        //Click the Create button and click the Template option.
        simPage.clickCreate();
        simPage.clickTemplate();

        //Verify default values are displayed in all fields.
        templateForm.checkResetFields("");

        //Enter a name for the composite in the Composite text box.
        templateForm.setCompositeName(composite);

        //Enter a name for the simulation into the Name text box.
        templateForm.setSimName(simulation);

        //Select a template from the Template dropdown.
        templateForm.selectTemplate(simulation);

        //Click the Create button.
        templateForm.clickCreate(true);

        //Verify the Create Simulation from Template modal disappears.
        Assert.assertFalse("Create Simulation from Template modal is still displayed despite clicking Create.", templateForm.isCreateSimTemplateHeaderDisplayed());

        //Verify the newly created composite and individual simulation are displayed in the list of simulations.
        Assert.assertTrue("New composite is not displayed despite successful creation.", simPage.isCompositeDisplayed(composite));
        Assert.assertTrue("New simulation is not displayed despite successful creation.", simPage.isSimDisplayed(simulation));

        //Verify the entered composite name and individual simulation name are displayed in the Name column.
        Assert.assertTrue("New composite name could not be found.", simPage.isCompositeNameDisplayed(composite.getName()));
        Assert.assertTrue("New simulation name could not be found.", simPage.isSimDisplayed(simulation.getName()));

        //Verify unique IDs are auto-generated and displayed for the composite and individual simulation in the ID column.
        Assert.assertNotNull("An ID was not assigned to the new composite as expected.", simPage.getCompositeID(composite));
        Assert.assertNotNull("An ID was not assigned to the new simulation as expected.", simPage.getSimID(simulation));

        //Verify the individual simulation type is displayed in the Type column.
        Assert.assertEquals("The value in the Type column for the newly created simulation is not displayed as expected.", simulation.getSimType().getLabel(), simPage.getSimType(simulation));

        //Verify the name of the selected template is associated with the individual simulation and displayed in the Source column.
        Assert.assertEquals("The value in the Source column for the newly created simulation is not displayed as expected.", "Template: " + simulation.getTemplate().getLabel(),
                simPage.getSimSource(simulation));

        //Log out
        simPage.logout(testUser);
    }

    /**
     * Test steps for ITC-072
     */
    @Test
    public void newCompositeFromTemplateInternal() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Click the Create button.
        simPage.clickCreate();

        //Click the Template option.
        CreateSimulationFromTemplateModal templateModal = simPage.clickTemplate();

        //With all fields blank, click the Create button in the Create Simulation from Template modal.
        templateModal.clickCreate(false);

        //Verify an error is displayed associated with the Composite text box indicating a valid composite name is required.
        Assert.assertTrue("Composite name field required error not displayed as expected when all fields are blank.", templateModal.isCompositeNameRequiredErrorDisplayed());

        //Verify an error is displayed associated with the Name text box indicating a valid simulation name is required.
        Assert.assertTrue("Simulation name field required error not displayed as expected when all fields are blank.", templateModal.isSimulationNameRequiredErrorDisplayed());

        //Verify an error is displayed associated with the Template dropdown indicating a valid template is required.
        Assert.assertTrue("Template field required error not displayed as expected when all fields are blank.", templateModal.isTemplateRequiredErrorDisplayed());

        //Verify the text “No template selected” is displayed next to the Template Description section.
        templateModal.checkNoTemplateSelectedText();

        //Enter a one more space characters in the Composite text box and enter valid values in the remaining fields
        templateModal.setAllFields(simulation);
        templateModal.setCompositeName("   ");

        //Click the Create button.
        templateModal.clickCreate(false);

        //Verify an error is displayed associated with the Composite text box indicating a valid composite is required.
        Assert.assertTrue("Composite name field required error not displayed as expected when whitespace only is entered in Composite Name text box.",
                templateModal.isCompositeNameRequiredErrorDisplayed());

        //Enter a value in the Composite text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        templateModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + "$(&*");

        //Verify an error is displayed associated with the Composite text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error not displayed as expected when special characters are used in Composite Name text box.", templateModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a value in the Composite text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        templateModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Composite text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error not displayed as expected when consecutive spaces are used in Composite Name text box.", templateModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a value in the Composite text box that begins with a space (e.g., ‘ Test’).
        templateModal.setCompositeName(" " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Composite text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error not displayed with Composite Name field as expected when leading whitespace is used in Composite Name text box.",
                templateModal.isCompositeNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Composite text box that ends with a space (e.g., ‘Test ’).
        templateModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + " ");

        //Verify an error is displayed associated with the Composite text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error not displayed with Composite Name field as expected when trailing whitespace is used in Composite Name text box.",
                templateModal.isCompositeNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Composite text box that begins with a hyphen/dash (e.g., ‘-Test’).
        templateModal.setCompositeName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Composite text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error not displayed as expected when a leading dash '-' is used in Composite Name text box.", templateModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a value in the Composite text box that ends with a hyphen/dash (e.g., ‘Test- ‘).
        templateModal.setCompositeName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Composite text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Composite Name error not displayed as expected when a trailing dash '-' is used in Composite Name text box.", templateModal.isCompositeNameInvalidErrorDisplayed());

        //Enter a valid name in the Composite text box.
        templateModal.setCompositeName(composite);

        //Enter one or more space characters in the Name text box.
        templateModal.setSimName("   ");

        //Verify an error is displayed associated with the Name text box indicating a valid simulation name is required.
        Assert.assertTrue("Simulation name field required error not displayed as expected when whitespace only is entered in Simulation Name text box.",
                templateModal.isSimulationNameRequiredErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).
        templateModal.setSimName(RandomStringGenerator.nextLetterString(5) + "$(&*");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error not displayed as expected when special characters are used in Simulation Name text box.",
                templateModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        templateModal.setSimName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error not displayed as expected when consecutive spaces are used in Simulation Name text box.",
                templateModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        templateModal.setSimName("  " + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error not displayed with Simulation Name field as expected when leading whitespace is used in Simulation Name text box.",
                templateModal.isSimulationNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’).
        templateModal.setSimName(RandomStringGenerator.nextLetterString(5) + "  ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Leading/Trailing Whitespace error not displayed with Simulation Name field as expected when trailing whitespace is used in Simulation Name text box.",
                templateModal.isSimulationNameLeadingTrailingSpacesErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g., ‘-Test’).
        templateModal.setSimName("-" + RandomStringGenerator.nextLetterString(5));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error not displayed as expected when a leading dash '-' is used in Simulation Name text box.", templateModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g., ‘Test-’).
        templateModal.setSimName(RandomStringGenerator.nextLetterString(5) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Simulation Name error not displayed as expected when a trailing dash '-' is used in Simulation Name text box.",
                templateModal.isSimulationNameInvalidErrorDisplayed());

        //Enter a valid name in the Name text box.
        templateModal.setSimName(simulation);

        //Click the Create button.
        templateModal.clickCreate(true);

        //Verify the newly created composite is displayed in the simulation list.
        Assert.assertTrue("Composite is not displayed as expected after valid values are used.", simPage.isCompositeDisplayed(composite));

        //Verify the newly created composite is selected, if not, select the newly created composite.
        if (!simPage.isCompositeSelected(composite)) {
            simPage.selectComposite(composite, true);
        }

        //Click the Create button.
        simPage.clickCreate();

        //Click the Template link.
        simPage.clickTemplate();

        //Enter the same name used in previous steps in the Name text box.
        templateModal.setSimName(simulation);

        //Select a valid from the Template dropdown.
        templateModal.selectTemplate(simulation);

        //Click the Create button.
        templateModal.clickCreate(false);

        //Verify an error is displayed associated with the Name text box indicating the simulation name must be unique.
        Assert.assertTrue(
                "Duplicate Simulation Name error not displayed as expected when name is used that matches the name of a simulation that already exists in the selected composite.",
                templateModal.isDuplicateSimulationNameErrorDisplayed(simulation));

        //Click the Cancel button.
        templateModal.clickCancel();

        //Log out
        simPage.logout(testUser);
    }
}
