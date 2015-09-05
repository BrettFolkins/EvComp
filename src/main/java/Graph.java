package com;

import com.DataSource;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.imageio.*;
import javax.swing.*;
import java.awt.FontMetrics;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.ArrayList;
import java.util.Timer;
import java.util.TimerTask;
import java.util.List;

public class Graph extends JPanel{
    private final static int RULER_XOFF = 10;
    private final static int RULER_YOFF =  2;
    private final static int REPAINT_INTERVAL = 50; //milliseconds
    private final static int NUM_HORZ_RULES = 4; //creates 2^NUM_VERT_RULES horizontal rulers
    private final static int NUM_VERT_RULES = 16; //creates x evenly spaces vertical rulers
    private final static boolean AA_ON = false; //anti-aliasing render hint
    private final static double XSCALE_MAX = 1.00;
    private final static double XSCALE_MIN = 0.05;

    private List<DataConfig> sources;
    private Timer refreshTimer;
    private GraphConfigWindow config;
    private double xScale  = XSCALE_MAX; //how much of the data's x range to display
    private double yScale  = 40.0; //scale of data per half graph height
    private double yCenter =  0.0; //pixel offset for where 0 line should be

    List<DataConfig> getSources(){ return sources; }
    void setXScale(double s) {
        xScale = s;
        if(config != null) config.graphConfigsUpdated();
    }
    void setYScale(double s) {
        yScale = s;
        if(config != null) config.graphConfigsUpdated();
    }
    void setYCenter(double s){
        yCenter= s;
        if(config != null) config.graphConfigsUpdated();
    }
    double getXScale() { return xScale;  }
    double getYScale() { return yScale;  }
    double getYCenter(){ return yCenter; }

    /*
    clean up graph logic
    add data/pixel units to parameters
    */

    public Graph(List<DataSource> inputSources){
        sources = new ArrayList<DataConfig>();
        for(DataSource source : inputSources){
            sources.add(new DataConfig(source));
        }

        //repaint at regular interval
        refreshTimer = new Timer();
        refreshTimer.scheduleAtFixedRate(new RefreshTimerTask(), 0, REPAINT_INTERVAL);

        //place configuration button
        this.setLayout(new FlowLayout(FlowLayout.LEADING));
        this.add(new JButton(configPopupAction));

        //close config window if the graph is closed
        this.addHierarchyListener(new HierarchyListener(){
            public void hierarchyChanged(HierarchyEvent e){
                if(isShowing()) return;
                if(config != null){
                    config.close();
                }
            }
        });

        //turn on graph mouse listener
        MouseAdapter mouseAdapter = new GraphMouseHandler();
        this.addMouseListener(mouseAdapter);
        this.addMouseMotionListener(mouseAdapter);
        this.addMouseWheelListener(mouseAdapter);
    }

    private class RefreshTimerTask extends TimerTask{
        public void run(){
            repaint();
        }
    }

    private Action configPopupAction = new AbstractAction(){
      {
        String text = "Configure";
        putValue(Action.NAME, text);
        putValue(Action.SHORT_DESCRIPTION, text);
      }
      public void actionPerformed(ActionEvent e){
        if(config == null){
            config = new GraphConfigWindow(Graph.this);
        }
        config.show();
      }
    };

    @Override
    public void paintComponent(Graphics g){
        super.paintComponent(g);

        //find data->pixel conversion constants
        final Graphics2D g2d = (Graphics2D) g;
        final double   scale =-getHeight()/yScale;
        final double  center = getHeight()/2+scale*-yCenter;

        //Draw background
        g2d.setPaint(Color.BLACK);
        drawGrid(g2d, scale, center);

        //Draw graph elements
        for(DataConfig data : sources){
            if(data.getDrawn())
                drawData(g2d, data, (float) scale, (float) center, (float) xScale);
                //drawData(g2d, data, scale, center, xScale);
        }
        drawLabels(g2d, sources);

        //Foreground draw by swing calling PaintComponents
    }

    private double pow2(int exp){
        int abs = Math.abs(exp);
        double output = (double)(1 << abs);
        if(exp < 0) return 1.0 / output;
        return output;
    }

    private void drawGrid(Graphics2D g2d, double scale, double center){
        Graphics2D g = (Graphics2D) g2d.create();
        /**
         * data*scale + center = pixel
         * (pixel - center) / scale = data
         * 0 is the top of the screen, height is the bottom
         */
        final double maxDataVal = (0.0-center)/scale;
        final double minDataVal = (getHeight()-center)/scale;
        final int    horzRuleScale = Math.getExponent(maxDataVal-minDataVal) - NUM_HORZ_RULES;
        final double horzRuleDelta = pow2(horzRuleScale);
        final double minRule = minDataVal - (minDataVal%horzRuleDelta);

        //horizontal rules
        g.setStroke(new BasicStroke(1));
        g.setColor(Color.LIGHT_GRAY);
        for(double r = minRule; r<maxDataVal; r+=horzRuleDelta){
            final int pY = (int)(r * scale + center);
            g.drawLine(0,pY,getWidth(),pY);
            g.drawString(""+r, RULER_XOFF, pY-RULER_YOFF);
        }

        //vertical rules
        g.setStroke(new BasicStroke(1));
        g.setColor(Color.LIGHT_GRAY);
        final int dx = getWidth()/NUM_VERT_RULES;
        for(int x=0; x<getWidth(); x+=dx){
            g.drawLine(x,0,x,getHeight());
        }

        //bold 0 line
        g.setStroke(new BasicStroke(3));
        g.setColor(Color.BLACK);
        g.drawLine(0, (int)center, getWidth(), (int)center); //bold 0 line

        g.dispose();
    }

    private void drawData(Graphics2D g2d, DataConfig data,
                          float scale, float center, float xWidth){
        Graphics2D g = (Graphics2D) g2d.create();
        if(AA_ON){
            RenderingHints rh = new RenderingHints(
                 RenderingHints.KEY_ANTIALIASING,
                 RenderingHints.VALUE_ANTIALIAS_ON);
            g.setRenderingHints(rh);
        }
        g.setPaint(data.getPaint());


        final DataSource source = data.getSource();
        final float width       = (float) this.getWidth();
        GeneralPath dataShape  = new GeneralPath();
        dataShape.moveTo(0,0);

        for(int x=0; x<width; x++){
            final float xPos = (1.0f-xWidth) + xWidth * (((float)x) / width);
            final float y = (float) (source.get(xPos)*scale + center);
            dataShape.lineTo(x, y);
        }

        g.draw(dataShape);

        g.dispose();
    }

    private void drawLabels(Graphics2D g2d, List<DataConfig> dcs){
        g2d = (Graphics2D) g2d.create();
        FontMetrics metrics = g2d.getFontMetrics();
        final int dropPoint = metrics.getDescent();
        final int rowHeight = metrics.getHeight()+dropPoint;

        final int RECT_BUFF = 2;
        final int WALL_BUFF = 10;
        final int TEXT_RISE = RECT_BUFF + 3;

        final int startX = WALL_BUFF;
        final int startY = getHeight() - WALL_BUFF;

        int dCount = 0;
        for(int i = 0; i < dcs.size(); i++) {
            final DataConfig dc = dcs.get(i);
            if(dc.getDrawn() == false) continue;
            final int left = startX;
            final int bot  = startY - rowHeight * dCount;
            dCount ++;
            g2d.setPaint(dc.getPaint());

            g2d.fillRect(left,
                            bot-rowHeight - RECT_BUFF,
                            rowHeight - RECT_BUFF,
                            rowHeight - RECT_BUFF*2);

            g2d.drawString(dc.getName(),
                            left+rowHeight, bot-dropPoint-TEXT_RISE);
        }
        g2d.dispose();
    }

    class DataConfig{
        DataSource  source;
        Paint       paint;
        boolean     drawn;
        public DataConfig(DataSource source, Boolean drawn){
            this.source = source;
            this.drawn = drawn;
            this.paint = (Paint) Color.BLACK;
        }
        public DataConfig(DataSource s){
            this(s, false);
        }
        public void setPaint(Paint p){
            this.paint = p;
        }
        public void setDrawn(Boolean draw){
            this.drawn = draw;
        }
        public Paint getPaint(){
            return this.paint;
        }
        public boolean getDrawn(){
            return this.drawn;
        }
        public DataSource getSource(){
            return source;
        }
        public String getName(){
            return source.getName();
        }
    }

    class GraphMouseHandler extends MouseAdapter{
        private Point dragPoint = null;
        @Override
        public void mousePressed(MouseEvent e){
            switch(e.getButton()){
                case MouseEvent.BUTTON1:
                    dragPoint = e.getPoint();
                    break;
                case MouseEvent.BUTTON2:
                    break;
                case MouseEvent.BUTTON3:
                    break;
                default:
                    break;
            }
        }
        @Override
        public void mouseDragged(MouseEvent e){
            if(dragPoint != null){
                final Graph   g = Graph.this;
                final double dx = e.getPoint().x - dragPoint.x;
                final double dy = e.getPoint().y - dragPoint.y;
                //scale x drag motion by width, use it to alter the graph x scale
                double xs = dx/((double)Graph.this.getWidth());
                xs += g.getXScale();
                xs = Math.max( Math.min(xs, XSCALE_MAX), XSCALE_MIN);
                g.setXScale( xs );
                //pan the y axis by delta y
                final double dyunit = (dy/g.getHeight()) * g.getYScale();
                g.setYCenter( g.getYCenter() + dyunit );
                //save current point for delta calculations
                dragPoint = e.getPoint();
            }
        }
        @Override
        public void mouseReleased(MouseEvent e){
            dragPoint = null;
        }
        @Override
        public void mouseWheelMoved(MouseWheelEvent e){
            final double ZOOM_FACTOR = 0.025;
            double d = e.getPreciseWheelRotation();
            Graph g = Graph.this;
            g.setYScale( d*ZOOM_FACTOR*g.getYScale() + g.getYScale() );
        }
    }

    //main method for quicker manual testing
    public static void main(String[] args) {
        List<DataSource> trialSources = new ArrayList<DataSource>();
        DataSource sin = new DataSource(){
            public double get(double x){
                return 20.0*Math.sin(x*3.0f*Math.PI);
            }
            public String getName(){
                return "sine";
            }
        };
        DataSource cos = new DataSource(){
            public double get(double x){
                return 20.0*Math.cos(x*3.0f*Math.PI);
            }
            public String getName(){
                return "cosine";
            }
        };
        trialSources.add(sin);
        trialSources.add(cos);

        Graph g = new Graph(trialSources);
        JFrame f = new JFrame("graphTest");
        f.add(g);
        f.pack();
        f.setVisible(true);

        //turn on all the test data sources
        for(DataConfig source : g.getSources()){
            source.setDrawn(true);
        }

        while(f.isShowing()){
            try {
                Thread.sleep(1000);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }
}
