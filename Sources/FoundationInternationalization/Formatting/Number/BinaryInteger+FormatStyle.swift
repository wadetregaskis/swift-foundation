//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if canImport(FoundationEssentials)
import FoundationEssentials
#endif

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#endif

@available(macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0, *)
extension BinaryInteger {

    /// Format `self` using `IntegerFormatStyle()`
    public func formatted() -> String {
        IntegerFormatStyle().format(Int(self))
    }

    /// Format `self` with the given format.
    public func formatted<S>(_ format: S) -> S.FormatOutput where Self == S.FormatInput, S: FormatStyle {
        format.format(self)
    }

    /// Format `self` with the given format. `self` is first converted to `S.FormatInput` type, then format with the given format.
    public func formatted<S>(_ format: S) -> S.FormatOutput where S: FormatStyle, S.FormatInput: BinaryInteger {
        format.format(S.FormatInput(self))
    }

}

// MARK: - BinaryInteger + Numeric string representation

extension BinaryInteger {
    
    /// Formats `self` in "Numeric string" format (https://speleotrove.com/decimal/daconvs.html)
    /// which is the required input form for certain ICU functions (e.g. `unum_formatDecimal`).
    ///
    /// This produces output that (at time of writing) looks identical to the `description` for
    /// many `BinaryInteger` types, such as the built-in integer types.  However, the format of
    /// `description` is not specifically defined by `BinaryInteger` (or anywhere else, really),
    /// and as such cannot be relied upon.  Thus this purpose-built method, instead.
    ///
    public var numericStringRepresentation: ArraySlice<UInt8> {
        numericStringRepresentationForBinaryInteger(words: self.words, isSigned: Self.isSigned)
    }
}

// MARK: - BinaryInteger + Numeric string representation + Utilities

/// The maximum [number of decimal digits][number-of-digits] needed the represent
/// an unsigned integer with the given `bitWidth`.
///
/// - Parameters:
///    - bitWidth: The bit width of an unsigned integer, which must be nonnegative.
///
/// [number-of-digits]: https://www.exploringbinary.com/number-of-decimal-digits-in-a-binary-integer
///
private func maxDecimalDigitCountForUnsignedInteger(bitWidth: Int) -> Int {
    Int((Double(UInt(bitWidth)) * log10(2)).rounded(.down)) + 1
}

/// The largest `exponent` and `power` in `pow(10, exponent) <= UInt.max + 1`.
///
/// The `exponent` is also the maximum number of decimal digits needed to represent a binary integer
/// in the range of `0 ..< power`. Another method is used to estimate the total number of digits, however.
/// This is so that binary integers can be rabased and encoded in the same loop.
///
/// ```
/// 32-bit: (exponent:  9, power:           1000000000)
/// 64-bit: (exponent: 19, power: 10000000000000000000)
/// ```
///
private func maxDecimalExponentAndPowerForUnsignedIntegerWord() -> (exponent: UInt, power: UInt) {
    var exponent = 1 as UInt
    var power = 0010 as UInt
    
    while true {
        let next = power.multipliedReportingOverflow(by: 10)
        if  next.overflow { break }
        
        exponent += 1
        power = next.partialValue
    }
    
    return (exponent: exponent, power: power)
}

/// Forms the `two's complement` of a binary integer's `words`.
///
/// - Parameters:
///    - words: A mutable collection of binary integer words, ordered from least significant to most significant.
///
private func formTwosComplementForBinaryInteger(words: UnsafeMutableBufferPointer<UInt>) {
    var carry =  true
    for index in words.indices {
        (words[index], carry) = (~words[index]).addingReportingOverflow(carry ? 1 : 0)
    }
}

/// Forms the `quotient` of dividing the `dividend` by the `divisor`, then returns the `remainder`.
///
/// - Parameters:
///   - dividend: An unsigned binary integer's words.
///   - divisor:  An unsigned binary integer's only word.
///
/// - Returns: The `remainder`, which is a value in the range of `0 ..< divisor`.
///
private func formQuotientWithRemainderForUnsignedInteger(
words dividend: Slice<UnsafeMutableBufferPointer<UInt>>, dividingBy divisor: UInt) -> UInt {
    var remainder = 0 as UInt
    
    for index in (dividend.startIndex ..< dividend.endIndex).reversed() {
        (dividend.base[index], remainder) = divisor.dividingFullWidth((high: remainder, low: dividend.base[index]))
    }
    
    return remainder
}

/// Writes `word` in "Numeric string" format (https://speleotrove.com/decimal/daconvs.html)  to
/// the trailing edge of `buffer`, then returns the least significant `buffer` index written to.
///
/// - Parameters:
///   - word: An unsigned binary integer's only word.
///   - buffer: A buffer with enough memory to accommodate the format.
///
private func formTrailingNumericStringRepresentationForUnsignedInteger(
word: UInt, into buffer: Slice<UnsafeMutableBufferPointer<UInt8>>) -> Int {
    var magnitude = word
    var index = buffer.endIndex
    
    repeat {
        precondition(index > buffer.startIndex)
        buffer.base.formIndex(before: &index)
        
        let remainder: UInt
        (magnitude, remainder) = magnitude.quotientAndRemainder(dividingBy: 10)
        buffer.base[index] = UInt8(ascii: "0") &+ UInt8(truncatingIfNeeded: remainder)
    }   while magnitude != 0
    
    return index as Int
}

/// Formats `words` in "Numeric string" format (https://speleotrove.com/decimal/daconvs.html)
/// which is the required input form for certain ICU functions (e.g. `unum_formatDecimal`).
///
/// - Parameters:
///   - words: The words of a binary integer.
///   - isSigned: The signedness of a binary integer.
///
internal func numericStringRepresentationForBinaryInteger(
words: some Collection<UInt>, isSigned: Bool) -> ArraySlice<UInt8> {
    withUnsafeTemporaryAllocation(of: UInt.self, capacity: words.count) { copy in
        // copies the words and then passes them to a non-generic, mutating, word-based algorithm
        let count =  copy.initialize(fromContentsOf: words)
        defer{ copy.baseAddress!.deinitialize(count: count) }
        return numericStringRepresentationForMutableBinaryInteger(words: copy, isSigned: isSigned)
    }
}

/// Formats `words` in "Numeric string" format (https://speleotrove.com/decimal/daconvs.html)
/// which is the required input form for certain ICU functions (e.g. `unum_formatDecimal`).
///
/// - Parameters:
///   - words: The mutable words of a binary integer.
///   - isSigned: The signedness of a binary integer.
///
/// This method consumes the `words` such that the buffer is filled with zeros when it returns.
///
private func numericStringRepresentationForMutableBinaryInteger(
words: UnsafeMutableBufferPointer<UInt>, isSigned: Bool) -> ArraySlice<UInt8> {
    //  negative values are in two's complement form
    let isLessThanZero = isSigned && Int(bitPattern: words.last ?? 0) < 0
    if  isLessThanZero {
        // forms the magnitude when it is less than zero
        formTwosComplementForBinaryInteger(words: words)
    }
    
    let radix =  maxDecimalExponentAndPowerForUnsignedIntegerWord()  as (exponent: UInt,  power: UInt)
    let capacity = maxDecimalDigitCountForUnsignedInteger(bitWidth:  max(1, words.count * UInt.bitWidth)) + (isLessThanZero ? 1 : 0)
    var ascii = ContiguousArray<UInt8>(repeating: UInt8(ascii: "0"), count: capacity) // fills the array with zeros (see later step)
    
    var wordsIndex = words.endIndex
    var writeIndex = ascii.endIndex
    var asciiIndex = ascii.endIndex
    
    ascii.withUnsafeMutableBufferPointer { ascii in
        repeat {
            // mutating buffer division prevents unnecessary allocations
            let remainder = formQuotientWithRemainderForUnsignedInteger(words: words.prefix(upTo: wordsIndex), dividingBy: radix.power)
            // the quotient is normalized for flexible-width performance
            wordsIndex = words.prefix(upTo: wordsIndex).reversed().drop(while:{ $0 == 0 }).startIndex.base
            // the remainder's numeric string representation is written to the array's trailing edge, up to the index
            writeIndex = formTrailingNumericStringRepresentationForUnsignedInteger(word: remainder, into: ascii.prefix(upTo: asciiIndex))
            // because the array is pre-filled with zeros, we can skip ahead. only the final loop's zeros are trimmed
            asciiIndex = asciiIndex - Int(bitPattern: radix.exponent)
            // the loop is done when the previously formed quotient is empty
        }   while wordsIndex > words.startIndex
        
        asciiIndex = writeIndex // branchlessly trims the final loop's zeros
        
        //  adds a minus sign when less than zero
        if  isLessThanZero {
            ascii.formIndex(before:  &asciiIndex)
            ascii[asciiIndex] = UInt8(ascii: "-")
        }
    }
    
    assert(words.allSatisfy({ $0 == 0 }))
    return ascii.suffix(from: asciiIndex) as ArraySlice<UInt8>
}

// MARK: - BinaryInteger + Parsing

@available(macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0, *)
extension BinaryInteger {
    /// Initialize an instance by parsing `value` with the given `strategy`.
    public init<S: ParseStrategy>(_ value: S.ParseInput, strategy: S) throws where S.ParseOutput : BinaryInteger {
        let parsed = try strategy.parse(value)
        self = Self(parsed)
    }

    public init<S: ParseStrategy>(_ value: S.ParseInput, strategy: S) throws where S.ParseOutput == Self {
        self = try strategy.parse(value)
    }

    public init(_ value: String, format: IntegerFormatStyle<Self>, lenient: Bool = true) throws {
        let parsed = try IntegerParseStrategy(format: format, lenient: lenient).parse(value)
        self = Self(parsed)
    }

    public init(_ value: String, format: IntegerFormatStyle<Self>.Percent, lenient: Bool = true) throws {
        let parsed = try IntegerParseStrategy(format: format, lenient: lenient).parse(value)
        self = Self(parsed)
    }

    public init(_ value: String, format: IntegerFormatStyle<Self>.Currency, lenient: Bool = true) throws {
        let parsed = try IntegerParseStrategy(format: format, lenient: lenient).parse(value)
        self = Self(parsed)
    }
}
