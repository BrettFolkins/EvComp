package com;

import com.TableColumn;

//import java.awt.*;
import java.awt.FlowLayout;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;

import java.lang.*;

public class ColumnTableModel extends AbstractTableModel{
	private List<TableColumn> columns;
	public ColumnTableModel(List<TableColumn> cList){
		columns = cList;
	}
	public int getColumnCount() {
        return columns.size();
    }
    public int getRowCount() {
    	int rowCount = Integer.MAX_VALUE;
    	Iterator itr = columns.iterator();
    	while(itr.hasNext()){
    		TableColumn col = (TableColumn) itr.next();
    		rowCount = Math.min(col.getRowCount(), rowCount);
    	}
        return rowCount;
    }
    public String getColumnName(int col) {
        return columns.get(col).getName();
    }
    public Object getValueAt(int row, int col) {
    	return columns.get(col).getValueAt(row);
    }
    public Class getColumnClass(int col) {
    	return columns.get(col).getDataClass();
    }
    public boolean isCellEditable(int row, int col) {
    	return columns.get(col).isRowEditable(row);
    }
    public void setValueAt(Object value, int row, int col) {
    	columns.get(col).setValueAt(value, row);
    	fireTableCellUpdated(row, col);
    }
}
