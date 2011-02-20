package elw.dp.ui;

import elw.dp.app.InstructionsTableModel;

import javax.swing.*;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import java.util.HashMap;
import java.util.Map;

public class RendererFactory {
	private final Map<JTable, Integer> accColCache = new HashMap<JTable, Integer>();

	protected int lookupAccessColumn(JTable table) {
		if (accColCache.get(table) != null) {
			return accColCache.get(table);
		}

		int accColIndex = findColIndexByName(table, InstructionsTableModel.COL_ACC);

		accColCache.put(table, accColIndex);
		return accColIndex;
	}

	private int findColIndexByName(JTable table, String colName) {
		final TableModel model = table.getModel();

		int accColIndex = -1;
		for (int i = 0; i < model.getColumnCount(); i++) {
			if (colName.equals(model.getColumnName(i))) {
				accColIndex = i;
			}
		}
		return accColIndex;
	}

	public TableColumn findColByName(JTable table, String colName) {
		final int colIndex = findColIndexByName(table, colName);

		if (colIndex >= 0) {
			return table.getColumnModel().getColumn(colIndex);
		}

		return null;
	}

	public void install(JTable jTable) {
		final TableColumnModel cModel = jTable.getColumnModel();
		for (int i = 0; i < cModel.getColumnCount(); i++) {
			final TableColumn column = cModel.getColumn(i);

			final TableCellRenderer colRenderer = column.getCellRenderer();
			final TableCellRenderer origRenderer;
			if (colRenderer == null) {
				origRenderer = jTable.getDefaultRenderer(jTable.getColumnClass(i));
			} else {
				origRenderer = colRenderer;
			}

			final AccessTrackingCellRenderer wrapper = new AccessTrackingCellRenderer(origRenderer, this);

			column.setCellRenderer(wrapper);
		}
	}
}
