package com.bounswe2017.group10.atlas.util;

import android.content.Context;
import android.graphics.Bitmap;
import android.renderscript.Allocation;
import android.renderscript.Element;
import android.renderscript.RenderScript;
import android.renderscript.ScriptIntrinsicBlur;

/**
 * This class gives the functionality of building blurring bitmap images with
 * default or manual blur parameters.
 */
public class BlurBuilder {
    private float bitmapScale;
    private float blurRadius;

    /**
     * Use this constructor if you want to set bitmapScale and blurRadius manually.
     *
     * @param bitmapScale Scale of the bitmap.
     * @param blurRadius Blur radius.
     */
    public BlurBuilder(float bitmapScale, float blurRadius) {
        this.bitmapScale = bitmapScale;
        this.blurRadius = blurRadius;
    }

    /**
     * Default constructor with reasonable default blurring parameters.
     */
    public BlurBuilder() {
        this(0.4f, 7.5f);  // default blur constants
    }

    /**
     * Blur a given bitmap image in a given context.
     * Code from https://stackoverflow.com/questions/31641973/how-to-blur-background-images-in-android
     *
     * @param context Context in which the image will be blurred.
     * @param image The bitmap image to be blurred.
     * @return Blurred image.
     */
    public Bitmap blur(Context context, Bitmap image) {
        int width = Math.round(image.getWidth()*this.bitmapScale);
        int height = Math.round(image.getHeight()*this.bitmapScale);

        Bitmap inputBitmap = Bitmap.createScaledBitmap(image, width, height, false);
        Bitmap outputBitmap = Bitmap.createBitmap(inputBitmap);

        RenderScript rs = RenderScript.create(context);
        ScriptIntrinsicBlur theIntrinsic = ScriptIntrinsicBlur.create(rs, Element.U8_4(rs));
        Allocation tmpIn = Allocation.createFromBitmap(rs, inputBitmap);
        Allocation tmpOut = Allocation.createFromBitmap(rs, outputBitmap);
        theIntrinsic.setRadius(this.blurRadius);
        theIntrinsic.setInput(tmpIn);
        theIntrinsic.forEach(tmpOut);
        tmpOut.copyTo(outputBitmap);

        return outputBitmap;
    }
}
