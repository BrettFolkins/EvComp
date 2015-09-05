package com;

import com.Graph;
import javax.swing.*;
import java.awt.*;
import javax.swing.event.*;
import java.util.List;
import java.util.ArrayList;
import javax.swing.colorchooser.AbstractColorChooserPanel;

class GraphConfigWindow{
    private Graph  subject; //the graph to configure
    private JFrame frame;
    private int closeupIndex;
    private JSpinner xScaleSpinner;
    private JSpinner yScaleSpinner;
    private JSpinner yCenterSpinner;

    public GraphConfigWindow(Graph subject){
        this.subject = subject;
        frame = new JFrame("Graph Configuration");

        JPanel container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));
        container.add(buildSpinners());
        container.add(buildSourceTable());
        container.add(buildCloseupPanel());

        frame.add(container);
        frame.pack();
    }

    public void show(){
        frame.setVisible(true);
    }

    public void close(){
        frame.dispose();
    }

    public void graphConfigsUpdated(){
        xScaleSpinner.setValue( subject.getXScale() );
        yScaleSpinner.setValue( subject.getYScale() );
        yCenterSpinner.setValue( subject.getYCenter() );
    }

    private JPanel buildSpinners(){
        JPanel spinnerPanel = new JPanel();

        //X scale spinner
        SpinnerNumberModel xScaleM = new SpinnerNumberModel(subject.getXScale(),
                                                           0.01, 1.0, 0.1);
        xScaleSpinner = new JSpinner(xScaleM);
        xScaleSpinner.addChangeListener(new ChangeListener(){
            public void stateChanged(ChangeEvent e){
                subject.setXScale(xScaleM.getNumber().doubleValue());
            }
        });

        //Y scale spinner
        SpinnerNumberModel yScaleM = new SpinnerNumberModel(subject.getYScale(),
                                                           0.01, 10000.0, 0.25);
        yScaleSpinner = new JSpinner(yScaleM);
        yScaleSpinner.addChangeListener(new ChangeListener(){
            public void stateChanged(ChangeEvent e){
                subject.setYScale(yScaleM.getNumber().doubleValue());
            }
        });

        //Y center spinner
        SpinnerNumberModel yCenterM = new SpinnerNumberModel(subject.getYCenter(),
                                                            -10000.0, 10000.0, 0.5);
        yCenterSpinner = new JSpinner(yCenterM);
        yCenterSpinner.addChangeListener(new ChangeListener(){
            public void stateChanged(ChangeEvent e){
                subject.setYCenter(yCenterM.getNumber().doubleValue());
            }
        });

        spinnerPanel.add(new JLabel("X Scale:"));
        spinnerPanel.add(xScaleSpinner);
        spinnerPanel.add(new JLabel(" Y Scale:"));
        spinnerPanel.add(yScaleSpinner);
        spinnerPanel.add(new JLabel(" Y Center:"));
        spinnerPanel.add(yCenterSpinner);

        return spinnerPanel;
    }

    private JComponent buildSourceTable(){
        List<Graph.DataConfig> sources = subject.getSources();

        ArrayList<TableColumn> cols = new ArrayList<TableColumn>();
        cols.add( new TableColumn(){
            public String  getName(){ return "#"; }
            public Object  getValueAt(int row){ return sources.get(row).getName(); }
            public int     getRowCount(){ return sources.size(); }
            public Class   getDataClass(){ return String.class; }
            public boolean isRowEditable(int row){ return false; }
            public void    setValueAt(Object val, int row){;}
        });
        cols.add( new TableColumn() {
            public String   getName(){ return "Graph?"; }
            public Object   getValueAt(int row){ return sources.get(row).getDrawn(); }
            public int      getRowCount(){ return sources.size(); }
            public Class    getDataClass(){ return Boolean.class; }
            public boolean  isRowEditable(int row){ return true; }
            public void     setValueAt(Object val, int row){
                if(val.getClass() == Boolean.class){
                    sources.get(row).setDrawn((Boolean) val);
                }
            }
        });

        ColumnTableModel colModel = new ColumnTableModel(cols);
        JTable table = new JTable(colModel);
        table.getSelectionModel().addListSelectionListener(new ListSelectionListener(){
        public void valueChanged(ListSelectionEvent event) {
                setCloseup(sources.get(table.getSelectedRow()));
            }
        });
        JScrollPane pane = new JScrollPane(table);
        table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        table.setFillsViewportHeight(true);
        table.setPreferredScrollableViewportSize(new Dimension(200, 120));
        pane.setBorder(BorderFactory.createCompoundBorder(
                         BorderFactory.createEmptyBorder(5, 10, 5,10),
                         BorderFactory.createLineBorder(Color.BLACK)  ));
        return pane;
    }

    private SpinnerNumberModel strokeModel;
    private Graph.DataConfig   closeupData;
    private JColorChooser      colorPicker;

    //provides a closeup view of the settings associated with a particular DataConfig
    private JPanel buildCloseupPanel(){
        JPanel panel = new JPanel();

        ChangeListener updateListener = new ChangeListener(){
            public void stateChanged(ChangeEvent e){
                updateCloseupPaint();
            }
        };

        strokeModel = new SpinnerNumberModel(1, 1, 100, 1);
        JSpinner lineStroke = new JSpinner(strokeModel);

        //construct color picker
        colorPicker = new JColorChooser(Color.BLACK);
        colorPicker.setPreviewPanel(new JPanel());
        colorPicker.getSelectionModel().addChangeListener(updateListener);
        panel.add(colorPicker.getChooserPanels()[0]);

        return panel;
    }

    private void setCloseup(Graph.DataConfig dc){
        closeupData = dc;
        colorPicker.setColor((Color)closeupData.getPaint());
    }

    private void updateCloseupPaint(){
        if(closeupData == null) return;
        closeupData.setPaint( (Paint) colorPicker.getColor() );
    }

}
