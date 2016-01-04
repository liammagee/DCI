// Global variables
float radius = 50.0;
int X, Y;
int nX, nY;
int delay = 16;

// Setup the Processing Canvas
void setup(){
  size( 800, 600 );
  strokeWeight( 10 );
  frameRate( 15 );
  X = width / 2;
  Y = height / 2;
  nX = X;
  nY = Y;  
}

// Main draw loop
void draw(){
  
}


// Set circle's next destination
void mouseMoved(){
  nX = mouseX;
  nY = mouseY;  
}

void drawText(String t) {
 background(#000033);
 
 // get the width for the text
 float twidth = textWidth(t);

 // place the text centered on the drawing area
 text(t, (width - twidth) / 2, 50);
}


void mouseClicked() {
  background(#000033);

  text(join(ages, "\n"), 50, 50);
}