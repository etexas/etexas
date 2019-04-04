/*-
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 - 2018 Harmonia Holdings Group, LLC
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
package org.etexascode.j2735_2016.frames;

import java.util.Objects;

import org.etexascode.j2735_2016.elements.RestrictionClassID;
import org.etexascode.j2735_2016.interfaces.UnalignedPackedEncodingRules;

/**
 * The restriction class assignment frame for the j2735 2016 specification.
 * 
 * @author ttevendale
 */
public class RestrictionClassAssignment implements UnalignedPackedEncodingRules {

    /**
     * The restriction class ID element.
     */
    private RestrictionClassID id;

    /**
     * The restriction user type list frame.
     */
    private RestrictionUserTypeList users;

    /**
     * A constructor setup mainly for decoding purposes.
     */
    public RestrictionClassAssignment() {

        id = new RestrictionClassID();
        users = new RestrictionUserTypeList();
    }

    /**
     * A constructor for the restriction class assignment.
     * 
     * @param id The restriction class ID element.
     * @param users The restriction user type list frame.
     */
    public RestrictionClassAssignment(RestrictionClassID id, RestrictionUserTypeList users) {

        this.id = Objects.requireNonNull(id);
        this.users = Objects.requireNonNull(users);
    }

    /**
     * A constructor for the restriction class assignment which allows primitive/enumerated data to
     * be passed to all primitive/enumerated elements.
     * 
     * @param id The restriction class ID value.
     * @param users The restriction user type list frame.
     */
    public RestrictionClassAssignment(int id, RestrictionUserTypeList users) {

        this.id = new RestrictionClassID(id);
        this.users = Objects.requireNonNull(users);
    }

    /**
     * A getter for the restriction class ID element.
     * 
     * @return The restriction class ID element.
     */
    public RestrictionClassID getId() {

        return id;
    }

    /**
     * A setter for the restriction class ID element.
     * 
     * @param id The restriction class ID element to set.
     */
    public void setId(RestrictionClassID id) {

        this.id = Objects.requireNonNull(id);
    }

    /**
     * A setter for the restriction class ID element. Allows primitive data to be passed to the
     * primitive element.
     * 
     * @param id The restriction class ID value to be set in the element.
     */
    public void setId(int id) {

        this.id.setValue(id);
    }

    /**
     * A getter for the restriction user type list frame.
     * 
     * @return The restriction user type list frame.
     */
    public RestrictionUserTypeList getUsers() {

        return users;
    }

    /**
     * A setter for the restriction user type list frame.
     * 
     * @param users The restriction user type list frame to set.
     */
    public void setUsers(RestrictionUserTypeList users) {

        this.users = Objects.requireNonNull(users);
    }

    @Override
    public String encodeUPER() {

        StringBuilder restrictionClassAssignmentBits = new StringBuilder();

        restrictionClassAssignmentBits.append(id.encodeUPER());
        restrictionClassAssignmentBits.append(users.encodeUPER());

        return restrictionClassAssignmentBits.toString();
    }

    @Override
    public String decodeUPER(String bits) {

        bits = id.decodeUPER(bits);
        bits = users.decodeUPER(bits);

        return bits;
    }

    @Override
    public int hashCode() {

        return Objects.hash(id, users);
    }

    @Override
    public boolean equals(Object object) {

        if (object == null) {

            return false;
        }
        if (!(object instanceof RestrictionClassAssignment)) {

            return false;
        }
        RestrictionClassAssignment frame = (RestrictionClassAssignment)object;
        return this.id.equals(frame.id)
                && this.users.equals(frame.users);
    }
}
