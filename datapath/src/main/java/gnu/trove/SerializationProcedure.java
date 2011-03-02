///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2002, Eric D. Friedman All Rights Reserved.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
///////////////////////////////////////////////////////////////////////////////

package gnu.trove;

import java.io.IOException;
import java.io.ObjectOutput;


/**
 * Implementation of the variously typed procedure interfaces that supports
 * writing the arguments to the procedure out on an ObjectOutputStream.
 * In the case of two-argument procedures, the arguments are written out
 * in the order received.
 *
 * <p>
 * Any IOException is trapped here so that it can be rethrown in a writeObject
 * method.
 * </p>
 *
 * Created: Sun Jul  7 00:14:18 2002
 *
 * @author Eric D. Friedman
 * @version $Id: SerializationProcedure.java,v 1.5 2006/11/10 23:27:54 robeden Exp $
 */

class SerializationProcedure implements
    TIntIntProcedure,
    TIntByteProcedure,
    TIntProcedure,
    TByteByteProcedure,
    TByteProcedure {

    private final ObjectOutput stream;
    IOException exception;

    SerializationProcedure ( ObjectOutput stream) {
        this.stream = stream;
    }

    public boolean execute(byte val) {
        try {
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int val) {
        try {
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, byte val) {
        try {
            stream.writeInt(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(int key, int val) {
        try {
            stream.writeInt(key);
            stream.writeInt(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }

    public boolean execute(byte key, byte val) {
        try {
            stream.writeByte(key);
            stream.writeByte(val);
        } catch (IOException e) {
            this.exception = e;
            return false;
        }
        return true;
    }
}
