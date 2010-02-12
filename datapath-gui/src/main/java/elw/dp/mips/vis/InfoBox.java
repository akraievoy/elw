/*
 * Copyright (c) Anton Kraievoy, IASA, Kiev, Ukraine, 2006.
 * This work is based on code of Dr. Dalton R. Hunkins, CS dept. of St. Bonaventure University, 2006.
 */
package elw.dp.mips.vis;

import java.awt.*;

public class InfoBox {

    int x1, y1, width, height;
    Color color;
    String line1, line2;

    public InfoBox(int x, int y, int w, Color c) {
        line1 = "";
        line2 = "";
        x1 = x;
        y1 = y;
        width = w;
        color = c;
    }

    void setInitPoint(int x, int y) {
        x1 = x;
        y1 = y;
    }

    void setLine1(String line) {
        line1 = line;
    }

    void setLine2(String line) {
        line2 = line;
    }

    void setWidth(int w) {
        width = w;
    }

    void draw(Graphics g) {
        g.setColor(color);
        if (line1.length() > 0 && line2.length() > 0) {
            height = 20;
            g.fillRect(x1, y1 - 2 * height, width, 2 * height);
            g.setColor(Color.black);
            g.drawString(line1, x1 + 5, y1 - height - 5);
            g.drawString(line2, x1 + 5, y1 - 5);
        } else if (line1.length() > 0) {
            height = 10;
            g.fillRect(x1, y1 - 2 * height, width, 2 * height);
            g.setColor(Color.black);
            g.drawString(line1, x1 + 5, y1 - 5);
        } else if (line2.length() > 0) {
            height = 10;
            g.fillRect(x1, y1 - 2 * height, width, 2 * height);
            g.setColor(Color.black);
            g.drawString(line2, x1 + 5, y1 - 5);
        }
    }
}
