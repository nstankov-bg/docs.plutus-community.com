import p5 from "p5";

const NAV_HEIGHT = 52;

class Star {
  public saturationPercentage: number;
  public saturated: boolean;

  constructor(
    public readonly color: p5.Color,
    public size: number,
    public readonly position: Record<string, number>
  ) {
    this.saturationPercentage = 0;
    this.saturated = false;
    this.size = size * (Math.random() + 0.5);
  }

  draw = (ctx: p5) => {
    const color = this.color;

    const alpha = (255 * this.saturationPercentage) / 100;

    color.setAlpha(alpha);
    ctx.fill(color);
    ctx.rect(
      this.position.x - 4,
      this.position.y,
      this.size + 4,
      this.size - 4,
      2
    );
    ctx.rect(
      this.position.x,
      this.position.y - 4,
      this.size - 4,
      this.size + 4,
      2
    );
  };

  saturate = () => {
    this.saturated = true;
  };

  desaturate = () => {
    this.saturated = false;
  };

  incrementSaturation = () => {
    this.saturationPercentage += 1;
  };

  decrementSaturation = () => {
    this.saturationPercentage -= 1;
  };
}

const calculateStarColor = (x: number, y: number, ctx: p5) => {
  const red = 100 + (155 * x) / ctx.windowWidth;
  const green = ctx.random(0, 255);
  const blue = 155 + (100 * y) / ctx.windowHeight;

  const bright = [
    ctx.random(200, 250),
    ctx.random(20, 220),
    ctx.random(10, 20),
  ];

  // low chance of returning white color
  if (ctx.random(100) > 92) return ctx.color(255, 255, 255);
  // low chance of returning a bright color
  if (ctx.random(100) > 86) return ctx.color(bright[0], bright[1], bright[2]);

  return ctx.color(red, green, blue);
};

const createStar = (x: number, y: number, ctx: p5) => {
  const SIZE = 8;
  const color = calculateStarColor(x, y, ctx);
  const position = { x, y };

  return new Star(color, SIZE, position);
};

const drawStars = (stars: Star[], ctx: p5) => {
  stars.map((star) => {
    const shouldSaturate = ctx.random(0, 10000) > 9992;

    if (star.saturated) {
      if (star.saturationPercentage === 100) {
        star.desaturate();
      } else {
        star.incrementSaturation();
      }
    } else {
      if (star.saturationPercentage === 0) {
        if (shouldSaturate) {
          star.saturate();
        }
      } else {
        star.decrementSaturation();
      }
    }

    star.draw(ctx);
  });
};

const populateStars = (stars: Star[], ctx: p5) => {
  const { windowWidth, windowHeight } = ctx;
  const aspectRatio = windowWidth / windowHeight;

  // remove all elements, if any
  stars.splice(0, stars.length);

  const rows = 10;
  const cols = Math.floor(rows * aspectRatio);
  const offsetX = windowWidth / (cols * 2) - 4;
  const offsetY = windowHeight / (rows * 2);

  for (let i = 0; i < cols; i++) {
    for (let j = 0; j < rows; j++) {
      const x = (windowWidth / cols) * i + offsetX;
      const y = (windowHeight / rows) * j + offsetY;

      stars.push(createStar(x, y, ctx));
    }
  }
};

const stars = (ctx: p5) => {
  const STARS: Star[] = [];

  ctx.setup = () => {
    ctx.createCanvas(ctx.windowWidth, ctx.windowHeight - NAV_HEIGHT);
    ctx.frameRate(30);
    ctx.stroke(100, 100, 100, 50);

    populateStars(STARS, ctx);
  };

  ctx.draw = () => {
    ctx.clear();
    drawStars(STARS, ctx);
  };

  ctx.windowResized = () => {
    ctx.resizeCanvas(ctx.windowWidth, ctx.windowHeight);
    populateStars(STARS, ctx);
  };
};

export default stars;
