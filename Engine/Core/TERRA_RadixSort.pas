Unit TERRA_RadixSort;

{$I terra.inc}

Interface
Uses TERRA_Utils;

Procedure radix_sort(Input:PCardinalArray; Count:Integer);

Implementation

// ---- number of elements to test (shows tradeoff of histogram size vs. sort size)
Const
  ct:Cardinal = 65536;

// ---- really, a correctness check, not correctness itself ;)
CORRECTNESS = 1;

// ---- use SSE prefetch (needs compiler support), not really a problem on non-SSE machines.
//		need http://msdn.microsoft.com/vstudio/downloads/ppack/default.asp
//		or recent VC to use this

(*
#if PREFETCH
#include <xmmintrin.h>	// for prefetch
#define pfval	64
#define pfval2	128
#define pf(x)	_mm_prefetch(cpointer(x + i + pfval), 0)
#define pf2(x)	_mm_prefetch(cpointer(x + i + pfval2), 0)
#else
#define pf(x)
#define pf2(x)
#endif*)

// ================================================================================================
// flip a float for sorting
//  finds SIGN of fp number.
//  if it's 1 (negative float), it flips all bits
//  if it's 0 (positive float), it flips the sign only
// ================================================================================================
Function FloatFlip(Const f:Cardinal):Cardinal;
Var
  Mask:Cardinal;
Begin
	mask := -Integer(f Shr 31) Or $80000000;
	Result := f Xor mask;
End;

Procedure FloatFlipX(Var f:Cardinal);
Var
  Mask:Cardinal;
Begin
	mask := -Integer(f Shr 31) Or $80000000;
	F := F Xor mask;
End;

// flip a float back (invert FloatFlip)
//  signed was flipped from above, so:
//  if sign is 1 (negative), it flips the sign bit back
//  if sign is 0 (positive), it flips all bits back
Function IFloatFlip(Const F:Cardinal):Cardinal;
Var
  Mask:Cardinal;
Begin
	mask := ((f Shr 31) - 1) Or $80000000;
	Result := F Xor mask;
End;

// ---- utils for accessing 11-bit quantities
(*Function _0(x:Cardinal):Cardinal; Begin Result :=	(x And $7FF)
Function _1(x:Cardinal)	((x Shr 11) And $7FF)
Function _2(x:Cardinal)	((x Shr 22) )*)

// ================================================================================================
// Main radix sort
// ================================================================================================
Procedure RadixSort11(finput, sorted:PSingleArray; Const elements:Cardinal);
Const
  kHist = 2048;
Var
  I, fi, si, ai, pos:Cardinal;
  Input, Sort:PCardinalArray;
	// 3 histograms on the stack:
  b0, b1, b2:Array[0..Pred(kHist)] Of Cardinal;
  sum0, sum1, sum2, tsum:Cardinal;
Begin
	sort := PCardinalArray(sorted);
	Input := PCardinalArray(fInput);


	For I:=0 To Pred(kHist) Do
  Begin
		b0[i] := 0;
    b1[i] := 0;
    b2[i] := 0;
	End;
	//memset(b0, 0, kHist * 12);

	// 1.  parallel histogramming pass
  I:=0;
  While I<Elements Do
  Begin
		//pf(Input);

		fi := FloatFlip(Input[i]);

		Inc(b0[(Fi And $7FF)]);
		Inc(b1[((Fi Shr 11) And $7FF)]);
		Inc(b2[Fi Shr 22]);

    Inc(I);
  End;

	// 2.  Sum the histograms -- each histogram entry records the number of values preceding itself.
  sum0 := 0;
  sum1 := 0;
  sum2 := 0;

  For I:=0 To Pred(kHist) Do
  Begin
		  tsum := b0[i] + sum0;
			b0[i] := sum0 - 1;
			sum0 := tsum;

			tsum := b1[i] + sum1;
			b1[i] := sum1 - 1;
			sum1 := tsum;

			tsum := b2[i] + sum2;
			b2[i] := sum2 - 1;
			sum2 := tsum;
  End;

	// byte 0: floatflip entire value, read/write histogram, write out flipped
  I := 0;
  While (I<elements) Do
  Begin
		fi := Input[i];
		FloatFlipX(fi);
		pos := (Fi And $7FF);

		//pf2(Input);
    Inc(b0[pos]);
		sort[b0[pos]] := fi;

    Inc(I);
  End;

	// byte 1: read/write histogram, copy
	//   sorted -> Input
  I := 0;
  While (I<elements) Do
  Begin
		si := sort[i];
		pos := (Si Shr 11) And $7FF;
		//pf2(sort);
    Inc(b1[pos]);
		Input[b1[pos]] := si;
    Inc(I);
  End;

	// byte 2: read/write histogram, copy & flip out
	//   Input -> sorted
  I := 0;
  While (I<elements) Do
  Begin
		ai := Input[i];
		pos := (ai Shr 22);

		//pf2(Input);
    Inc(b2[pos]);
		sort[b2[pos]] := IFloatFlip(ai);
    Inc(I);
  End;

	// to write original:
	// memcpy(Input, sorted, elements * 4);
End;


(**
 *	Revisited Radix Sort.
 *	This is my new radix routine:
 *  - it uses indices and doesn't recopy the values anymore, hence wasting less ram
 *  - it creates all the histograms in one run instead of four
 *  - it sorts words faster than dwords and bytes faster than words
 *  - it correctly sorts negative floating-point values by patching the offsets
 *  - it automatically takes advantage of temporal coherence
 *  - multiple keys support is a side effect of temporal coherence
 *  - it may be worth recoding in asm... (mainly to use FCOMI, FCMOV, etc) [it's probably memory-bound anyway]
 *
 *	History:
 *	- 08.15.98: very first version
 *	- 04.04.00: recoded for the radix article
 *	- 12.xx.00: code lifting
 *	- 09.18.01: faster CHECK_PASS_VALIDITY thanks to Mark D. Shattuck (who provided other tips, not included here)
 *	- 10.11.01: added local ram support
 *	- 01.20.02: bugfix! In very particular cases the last pass was skipped in the float code-path, leading to incorrect sorting......
 *
 *	\class		RadixSort
 *	\author		Pierre Terdiman
 *	\version	1.3
 *	\date		August, 15, 1998
 * )

(*Var
{$IFNDEF RADIX_LOCAL_RAM}
				mHistogram:Array[0..Pred(256*4)] Of Cardinal;					//!< Counters for each byte
				mOffset:Array[0..255] Of Cardinal;					//!< Offsets (nearly a cumulative distribution function)
{$ENDIF}
				mCurrentSize:Cardinal;				//!< Current size of the indices list
				mPreviousSize:Cardinal;				//!< Size involved in previous call
				mIndices:Array Of Cardinal;					//!< Two lists, swapped each pass
				mIndices2:Array Of Cardinal;


Procedure CREATE_HISTOGRAMS(input, buffer:PCardinalArray);
Type
  TT = Cardinal;
Var
  PrevVal, Val:TT;
  AlreadySorted:Boolean;
  Indices:PCardinal;
  P, PE:PByte;
  H0, H1, H2, H3:PCardinal;
Begin
	// Clear counters
	FillChar(mHistogram, 256*4*sizeof(Cardinal), 0);

	// Prepare for temporal coherence */
	PrevVal := TT(buffer[mIndices[0]]);
	AlreadySorted := True;	// Optimism... *
	Indices := @mIndices[0];

	// Prepare to count
	p := PByte(input[0]);
	pe := @(p[nb*4]);
	h0 := @mHistogram[0];		// Histogram for first pass (LSB)
	h1 := @mHistogram[256];	// Histogram for second pass
	h2 := @mHistogram[512];	// Histogram for third pass
	h3 := @mHistogram[768];	// Histogram for last pass (MSB)

	while (p<>=pe) Do
  Begin
		// Read input buffer in previous sorted order
		Val := TT(buffer[Indices^]);
    Indices++;
		// Check whether already sorted or not
		If (Val<PrevVal) Then
    Begin
      AlreadySorted := False;
      Break;
     End; // Early out

		//Update for next iteration
		PrevVal := Val;

		// Create histograms
		Inc(h0[p^]); Inc(P);
    Inc(h1[p^]); Inc(P);
    Inc(h2[p^]); Inc(P);
    Inc(h3[p^]); Inc(P);
  End;

	// If all input values are already sorted, we just have to return and leave the
	// previous list unchanged. That way the routine may take advantage of temporal
	// coherence, for example when used to sort transparent faces.

	If(AlreadySorted)	Then
    Result := Nil; //!!!!!!!!!!!!!!!!

	// Else there has been an early out and we must finish computing the histograms
	While (p<>pe) Do
	Begin
		// Create histograms without the previous overhead
		Inc(h0[p^]); Inc(P);
    Inc(h1[p^]); Inc(P);
    Inc(h2[p^]); Inc(P);
    Inc(h3[p^]); Inc(P);
	End;
End;

Procedure CHECK_PASS_VALIDITY(pass:Cardinal);
Var
  CurCount:PCardinalArray;
  PerformPass:Boolean;
Begin
	// Shortcut to current counters
	CurCount := @mHistogram[pass Shl 8];

	// Reset flag. The sorting pass is supposed to be performed. (default)
	PerformPass := true;

	// Check pass validity

	// If all values have the same byte, sorting is useless.
	// It may happen when sorting bytes or words instead of dwords.
	// This routine actually sorts words faster than dwords, and bytes
	// faster than words. Standard running time (O(4*n))is reduced to O(2*n)
	// for words and O(n) for bytes. Running time for floats depends on actual values...
																								\
	/* Get first byte */
	ubyte UniqueVal = *(((ubyte* )input)+pass);													\
																								\
	/* Check that byte's counter */																\
	if(CurCount[UniqueVal]==nb)	PerformPass=false;


**
 *	Main sort routine.
 *	This one is for integer values. After the call, mIndices contains a list of indices in sorted order, i.e. in the order you may process your data.
 *	\param		input			[in] a list of integer values to sort
 *	\param		nb				[in] number of values to sort
 *	\param		signedvalues	[in] true to handle negative values, false if you know your input buffer only contains positive values
 *	\return		Self-Reference
 */
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
RadixSort& RadixSort::Sort(const udword* input, udword nb, bool signedvalues)
{
	// Checkings
	if(!input || !nb)	return *this;

	// Stats
	mTotalCalls++;

	// Resize lists if needed
	CHECK_RESIZE(nb);

#ifdef RADIX_LOCAL_RAM
	// Allocate histograms & offsets on the stack
	udword mHistogram[256*4];
	udword mOffset[256];
#endif

	// Create histograms (counters). Counters for all passes are created in one run.
	// Pros:	read input buffer once instead of four times
	// Cons:	mHistogram is 4Kb instead of 1Kb
	// We must take care of signed/unsigned values for temporal coherence.... I just
	// have 2 code paths even if just a single opcode changes. Self-modifying code, someone?
	if(!signedvalues)	{ CREATE_HISTOGRAMS(udword, input);	}
	else				{ CREATE_HISTOGRAMS(sdword, input);	}

	// Compute #negative values involved if needed
	udword NbNegativeValues = 0;
	if(signedvalues)
	{
		// An efficient way to compute the number of negatives values we'll have to deal with is simply to sum the 128
		// last values of the last histogram. Last histogram because that's the one for the Most Significant Byte,
		// responsible for the sign. 128 last values because the 128 first ones are related to positive numbers.
		udword* h3= &mHistogram[768];
		for(udword i=128;i<256;i++)	NbNegativeValues += h3[i];	// 768 for last histogram, 128 for negative part
	}

	// Radix sort, j is the pass number (0=LSB, 3=MSB)
	for(udword j=0;j<4;j++)
	{
		CHECK_PASS_VALIDITY(j);

		// Sometimes the fourth (negative) pass is skipped because all numbers are negative and the MSB is 0xFF (for example). This is
		// not a problem, numbers are correctly sorted anyway.
		if(PerformPass)
		{
			// Should we care about negative values?
			if(j!=3 || !signedvalues)
			{
				// Here we deal with positive values only

				// Create offsets
				mOffset[0] = 0;
				for(udword i=1;i<256;i++)		mOffset[i] = mOffset[i-1] + CurCount[i-1];
			}
			else
			{
				// This is a special case to correctly handle negative integers. They're sorted in the right order but at the wrong place.

				// Create biased offsets, in order for negative numbers to be sorted as well
				mOffset[0] = NbNegativeValues;												// First positive number takes place after the negative ones
				for(udword i=1;i<128;i++)		mOffset[i] = mOffset[i-1] + CurCount[i-1];	// 1 to 128 for positive numbers

				// Fixing the wrong place for negative values
				mOffset[128] = 0;
				for(i=129;i<256;i++)			mOffset[i] = mOffset[i-1] + CurCount[i-1];
			}

			// Perform Radix Sort
			ubyte* InputBytes	= (ubyte* )input;
			udword* Indices		= mIndices;
			udword* IndicesEnd	= &mIndices[nb];
			InputBytes += j;
			while(Indices!=IndicesEnd)
			{
				udword id = *Indices++;
				mIndices2[mOffset[InputBytes[id<<2]]++] = id;
			}

			// Swap pointers for next pass. Valid indices - the most recent ones - are in mIndices after the swap.
			udword* Tmp	= mIndices;	mIndices = mIndices2; mIndices2 = Tmp;
		}
	}
	return *this;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 *	Main sort routine.
 *	This one is for floating-point values. After the call, mIndices contains a list of indices in sorted order, i.e. in the order you may process your data.
 *	\param		input			[in] a list of floating-point values to sort
 *	\param		nb				[in] number of values to sort
 *	\return		Self-Reference
 *	\warning	only sorts IEEE floating-point values
 */
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
RadixSort& RadixSort::Sort(const float* input2, udword nb)
{
	// Checkings
	if(!input2 || !nb)	return *this;

	// Stats
	mTotalCalls++;

	udword* input = (udword* )input2;

	// Resize lists if needed
	CHECK_RESIZE(nb);

#ifdef RADIX_LOCAL_RAM
	// Allocate histograms & offsets on the stack
	udword mHistogram[256*4];
	udword mOffset[256];
#endif

	// Create histograms (counters). Counters for all passes are created in one run.
	// Pros:	read input buffer once instead of four times
	// Cons:	mHistogram is 4Kb instead of 1Kb
	// Floating-point values are always supposed to be signed values, so there's only one code path there.
	// Please note the floating point comparison needed for temporal coherence! Although the resulting asm code
	// is dreadful, this is surprisingly not such a performance hit - well, I suppose that's a big one on first
	// generation Pentiums....We can't make comparison on integer representations because, as Chris said, it just
	// wouldn't work with mixed positive/negative values....
	{ CREATE_HISTOGRAMS(float, input2); }

	// Compute #negative values involved if needed
	udword NbNegativeValues = 0;
	// An efficient way to compute the number of negatives values we'll have to deal with is simply to sum the 128
	// last values of the last histogram. Last histogram because that's the one for the Most Significant Byte,
	// responsible for the sign. 128 last values because the 128 first ones are related to positive numbers.
	udword* h3= &mHistogram[768];
	for(udword i=128;i<256;i++)	NbNegativeValues += h3[i];	// 768 for last histogram, 128 for negative part

	// Radix sort, j is the pass number (0=LSB, 3=MSB)
	for(udword j=0;j<4;j++)
	{
		// Should we care about negative values?
		if(j!=3)
		{
			// Here we deal with positive values only
			CHECK_PASS_VALIDITY(j);

			if(PerformPass)
			{
				// Create offsets
				mOffset[0] = 0;
				for(udword i=1;i<256;i++)		mOffset[i] = mOffset[i-1] + CurCount[i-1];

				// Perform Radix Sort
				ubyte* InputBytes	= (ubyte* )input;
				udword* Indices		= mIndices;
				udword* IndicesEnd	= &mIndices[nb];
				InputBytes += j;
				while(Indices!=IndicesEnd)
				{
					udword id = *Indices++;
					mIndices2[mOffset[InputBytes[id<<2]]++] = id;
				}

				// Swap pointers for next pass. Valid indices - the most recent ones - are in mIndices after the swap.
				udword* Tmp	= mIndices;	mIndices = mIndices2; mIndices2 = Tmp;
			}
		}
		else
		{
			// This is a special case to correctly handle negative values
			CHECK_PASS_VALIDITY(j);

			if(PerformPass)
			{
				// Create biased offsets, in order for negative numbers to be sorted as well
				mOffset[0] = NbNegativeValues;												// First positive number takes place after the negative ones
				for(udword i=1;i<128;i++)		mOffset[i] = mOffset[i-1] + CurCount[i-1];	// 1 to 128 for positive numbers

				// We must reverse the sorting order for negative numbers!
				mOffset[255] = 0;
				for(i=0;i<127;i++)		mOffset[254-i] = mOffset[255-i] + CurCount[255-i];	// Fixing the wrong order for negative values
				for(i=128;i<256;i++)	mOffset[i] += CurCount[i];							// Fixing the wrong place for negative values

				// Perform Radix Sort
				for(i=0;i<nb;i++)
				{
					udword Radix = input[mIndices[i]]>>24;								// Radix byte, same as above. AND is useless here (udword).
					// ### cmp to be killed. Not good. Later.
					if(Radix<128)		mIndices2[mOffset[Radix]++] = mIndices[i];		// Number is positive, same as above
					else				mIndices2[--mOffset[Radix]] = mIndices[i];		// Number is negative, flip the sorting order
				}
				// Swap pointers for next pass. Valid indices - the most recent ones - are in mIndices after the swap.
				udword* Tmp	= mIndices;	mIndices = mIndices2; mIndices2 = Tmp;
			}
			else
			{
				// The pass is useless, yet we still have to reverse the order of current list if all values are negative.
				if(UniqueVal>=128)
				{
					for(i=0;i<nb;i++)	mIndices2[i] = mIndices[nb-i-1];

					// Swap pointers for next pass. Valid indices - the most recent ones - are in mIndices after the swap.
					udword* Tmp	= mIndices;	mIndices = mIndices2; mIndices2 = Tmp;
				}
			}
		}
	}
	return *this;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 *	Resets the inner indices. After the call, mIndices is reset.
 */
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void RadixSort::ResetIndices()
{
	for(udword i=0;i<mCurrentSize;i++)	mIndices[i] = i;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**
 *	Gets the ram used.
 *	\return		memory used in bytes
 */
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
udword RadixSort::GetUsedRam() const
{
	udword UsedRam = sizeof(RadixSort);
#ifndef RADIX_LOCAL_RAM
	UsedRam += 256*4*sizeof(udword);			// Histograms
	UsedRam += 256*sizeof(udword);				// Offsets
#endif
	UsedRam += 2*mCurrentSize*sizeof(udword);	// 2 lists of indices
	return UsedRam;
}
*)

Procedure byteRadixSort(Dest,source:PCardinalArray; Count, ShiftCount:integer);
Var
  Histogram : array [0..255] of integer;
  OffsetTable : array [0..255] of integer;
  Index : integer;
  Value, ValueToIndex : Longword;
Begin
  // biuld the histogram
  fillchar(Histogram[0], sizeof(Histogram), 0 );
  For index := 0 to Pred(Count) Do
  begin
    Value := source[index];
    ValueToIndex := ( Value shr ShiftCount ) and $FF;
    inc( Histogram[ ValueToIndex ] );
  end;

  // biuld the offsettable
  OffsetTable[0] := 0;
  for index := 1 to 255 do
  OffsetTable[index] := OffsetTable[index-1] + Histogram[index-1];

  // do the inner loop of the radix sort
  for index := 0 to Pred(Count) do
  begin
    Value := source[index];
    ValueToIndex := ( value shr ShiftCount ) and $FF;
    dest[ OffsetTable[ ValueToIndex ] ] := Value;
    inc( OffsetTable[ ValueToIndex ] );
  end;
End;

Procedure radix_sort(Input:PCardinalArray; Count:Integer);
Var
  tmp:Array Of Cardinal;
  I:Integer;
Begin
  // create the tmp buffer
  setlength(tmp, Count);

  // sort based on the 1st byte
  byteRadixSort(@tmp[0], @Input[0], Count, 0 );

  // sort based on the 2nd byte
  byteRadixSort(@Input[0], @tmp[0], Count, 8);

  // sort based on the 3rd byte
  byteRadixSort(@tmp[0], @Input[0], Count, 16);

  // sort based on the 4th byte
  byteRadixSort(@Input[0], @tmp[0], Count, 24);
End;

End.
