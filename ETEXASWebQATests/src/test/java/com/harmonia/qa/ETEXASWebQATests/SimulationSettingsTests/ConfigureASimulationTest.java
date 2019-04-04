package com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests;

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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.EditOptions;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.SimBtns;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;

/**
 * Test class which executes steps for the Configure a Simulation test, TC-095
 *
 * @author llaroussini
 */
public class ConfigureASimulationTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation testSim;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        //Get test user and test simulation
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        testSim = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = testSim.getComposite();
        ETexasEntityManager.addEntities(testUser, testSim);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(testSim);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-095
     */
    @Test
    public void configureSimExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
        //Ensure the Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a simulation within a composite that has no executions.
        simPage.expandComposite(composite, true);
        simPage.selectSim(testSim, true);

        //Verify the Edit button is enabled.
        Assert.assertTrue("Edit button is not enabled as expected when a simulation is selected.", simPage.isBtnEnabled(SimBtns.EDIT_BTN));

        //Click the Edit button.
        simPage.clickEdit();

        //Verify the Simulation Settings option is enabled.
        Assert.assertTrue("The Simulation Settings option in the Edit menu is not enabled when a simulation is selected.", simPage.isEditOptionEnabled(EditOptions.SIMULATION_SETTINGS));

        //Click the Simulation Settings option and verify the Simulation Settings modal is displayed.
        SimulationSettingsModal simSettingsModal = simPage.clickSimulationSettings();
        simSettingsModal.checkHeader();

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        simSettingsModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        simSettingsModal.clickSimSettingsHelpIcon();

        //Verify a Simulation Settings Help modal is displayed with instructions for configuring simulations.
        simSettingsModal.checkHelpModal();

        //Verify an OK button is displayed in the modal and click the OK button.
        Assert.assertTrue("An OK button is not displayed in the Simulation Settings Help window.", simSettingsModal.isSimSettingsHelpOKBtnDisplayed());
        simSettingsModal.clickSimSettingsHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("Simulation Settings Help header is still displayed after clicking OK to close.", simSettingsModal.isSimSettingsHelpHeaderDisplayed());

        //Verify the following tabs are displayed in the Simulation Settings modal: Cell Towers, Detectors, Fixed Cellular Devices, and RSE Devices.
        simSettingsModal.checkTabs();

        //Verify a Close button is displayed at the bottom of the modal.
        Assert.assertTrue("Close button is not displayed in the Simulation Settings modal as expected.", simSettingsModal.isCloseBtnDisplayed());

        //Click the Close button.
        simPage = simSettingsModal.clickClose();

        //Verify the Simulation Settings modal is no longer displayed.
        Assert.assertFalse("Simulation Settings header is still displayed after clicking the Close button.", simSettingsModal.isSimSettingsHeaderDisplayed());

        //Select a simulation within a composite that has no executions.
        if (simPage.isSimDisplayed(testSim) == false) {
            simPage.expandComposite(composite, true);
        }
        simPage.selectSim(testSim, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Simulation Settings option and verify the Simulation Settings modal is displayed.
        simSettingsModal = simPage.clickSimulationSettings();
        simSettingsModal.checkHeader();

        //Click the ‘x’ icon.
        simPage = simSettingsModal.clickCloseIcon();

        //Verify the Simulation Settings modal is no longer displayed.
        Assert.assertFalse("Simulation Settings header is still displayed after clicking the Close button.", simSettingsModal.isSimSettingsHeaderDisplayed());

        //Log out
        simPage.logout(testUser);
    }
}
