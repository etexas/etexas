package com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;

/**
 * Test class which executes steps for the Delete a Composite Simulation test,
 * TC-085
 *
 * @author llaroussini
 */
public class DeleteCompositeSimulationTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation simulation;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Test setup
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user and test simulation
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //gets a random template simulation
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testUser, simulation, composite);

        //Register user and create new simulation from template
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(simulation);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-085
     */
    @Test
    public void deleteCompositeTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify the Simulations page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select an existing composite.
        simPage.selectComposite(composite, true);

        //Click the Delete button.
        ConfirmDeleteModal deleteModal = simPage.clickDelete();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected composite.
        deleteModal.checkConfirmDeleteHeader();
        deleteModal.checkDeleteWarningCompositeContent(composite);

        //Verify a ‘x’ icon is displayed in the upper right corner of the Confirm Delete modal.
        Assert.assertTrue("Close icon not displayed as expected in Confirm Delete modal.", deleteModal.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the Confirm Delete modal.
        deleteModal.checkConfirmDeleteBtns();

        //Click the No button.
        deleteModal.clickBtn(Btn.NO);

        //Verify the Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Confirm Delete modal is still displayed after clicking 'No'.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the composite is still displayed.
        Assert.assertTrue("The composite is not displayed after clicking 'No'.", simPage.isCompositeDisplayed(composite));

        //Click the Delete button again.
        simPage.clickDelete();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkConfirmDeleteHeader();

        //Click the ‘x’ icon.
        deleteModal.clickCloseIcon();

        //Verify the Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Confirm Delete modal is still displayed after clicking the Close icon.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the composite is still displayed.
        Assert.assertTrue("The composite is not displayed after clicking the Close icon.", simPage.isCompositeDisplayed(composite));

        //Click the Delete button again.
        simPage.clickDelete();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkConfirmDeleteHeader();

        //Click the Yes button.
        deleteModal.clickBtn(Btn.YES);

        //Verify the Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Confirm Delete modal is still displayed after clicking 'Yes'.", deleteModal.isConfirmDeleteHeaderDisplayed());

        //Verify the composite is no longer displayed.
        Assert.assertFalse("The composite is still displayed after confirming deletion.", simPage.isCompositeDisplayed(composite));

        //Verify all simulations associated with the composite are no longer displayed.
        if (composite.getTemplateSims().size() != 0) {
            for (TemplateSimulation templateSim : composite.getTemplateSims()) {
                Assert.assertFalse("The simulation named: " + templateSim.getName() + " is still displayed after confirming deletion of it's parent composite.", simPage.isSimDisplayed(templateSim));
            }
        }
        if (composite.getUploadedSims().size() != 0) {
            for (UploadedSimulation uploadedSim : composite.getUploadedSims()) {
                Assert.assertFalse("The simulation named: " + uploadedSim.getName() + " is still displayed after confirming deletion of it's parent composite.", simPage.isSimDisplayed(uploadedSim));
            }
        }

        //Log out
        simPage.logout(testUser);
    }
}
