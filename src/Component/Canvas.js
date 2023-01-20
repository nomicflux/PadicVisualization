function setStrokeGradientStyle(ctx) {
  return function(grad) {
    return function () {
      ctx.strokeStyle = grad;
    };
  };
}

export { setStrokeGradientStyle };
