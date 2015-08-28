#pragma once

#include <string>
#include <vector>
#include <algorithm>

#include "assert.hpp"
#include "printable.hpp"


class TxIdentifier : public Printable {
    std::string ns;
    std::vector<std::string> segments;

public:
    /** Constructs an empty identifier (""). */
    TxIdentifier()  { }  // currently we treat this as legal

    /** Constructs a TxIdentifier whose contents are parsed from the provided string.
     * (split on the '.' characters)
     */
    TxIdentifier(const char* input) : TxIdentifier(std::string(input))  { }

    /** Constructs a TxIdentifier whose contents are parsed from the provided string.
     * (split on the '.' characters)
     */
    TxIdentifier(const std::string& input) {
        if (input.empty())
            return;  // currently we treat this as legal
        size_t lastPos = 0;
        while (true) {
            size_t pos = input.find_first_of('.', lastPos);
            if (pos == std::string::npos) {
                this->append(input.substr(lastPos));  // rest of string
                break;
            }
            this->append(input.substr(lastPos, (pos-lastPos)));  // next segment
            lastPos = pos+1;  // skip the period
        }
    }

    /** Constructs a TxIdentifier that extends the base namespace with another name segment. */
    TxIdentifier(const TxIdentifier& base, const std::string& name) : TxIdentifier(base) {
        this->append(name);
    }

    /** Constructs a TxIdentifier that extends the base namespace with another (moved) name segment. */
    TxIdentifier(const TxIdentifier& base, std::string&& name) : TxIdentifier(base) {
        this->append(name);
    }

    /** Constructs a TxIdentifier that extends the (moved) base namespace with another (moved) name segment. */
    TxIdentifier(TxIdentifier&& base, std::string&& name) : TxIdentifier(base) {
        this->append(name);
    }

    /** Constructs a TxIdentifier that is a subsequence of the provided namespace. */
    TxIdentifier(const TxIdentifier& other, int startIndex, int endIndex=-1) {
        int termIx = other.segment_count();
        if (startIndex < 0)
            startIndex += termIx;
        if (endIndex < 0)
            endIndex += termIx+1;
        if (startIndex < 0 || startIndex > termIx)
            throw std::out_of_range("startIndex out of range [0," + std::to_string(termIx) + "): " + std::to_string(startIndex));
        if (endIndex < 0 || endIndex > termIx)
            throw std::out_of_range("endIndex out of range [0," + std::to_string(termIx) + "): " + std::to_string(endIndex));
        for (int i = startIndex; i < endIndex; i++)
            this->append(other.segments[i]);
    }

    /** Copy constructor. */
    TxIdentifier(const TxIdentifier& other) : ns(other.ns), segments(other.segments) {
    }

    /** Move constructor. */
    TxIdentifier(TxIdentifier&& other) : ns(std::move(other.ns)), segments(std::move(other.segments)) {
    }

    virtual ~TxIdentifier() = default;


    /* Copy assignment operator. */
    TxIdentifier& operator=(const TxIdentifier& other) {
        if (this != &other) {
            this->ns = other.ns;
            this->segments = other.segments;
        }
        return *this;
    }

    /* Move assignment operator. */
    TxIdentifier& operator=(TxIdentifier&& other) {
        if (this != &other) {
            this->ns = std::move(other.ns);
            this->segments = std::move(other.segments);
        }
        return *this;
    }


    /** Appends all the segments of another identifier. */
    void appendIdent(const TxIdentifier& other) {
        for (auto s : other.segments)
            this->append(s);
    }


    /** Appends a segment to this identifier. */
    void append(const std::string& newSegment) {
        ASSERT(!newSegment.empty(), "provided segment is empty");
        ASSERT(newSegment.find_first_of('.') == std::string::npos, "segment may not contain a '.': " << newSegment);
        this->segments.push_back(newSegment);
        if (! this->ns.empty())
            this->ns.append(".");
        this->ns.append(newSegment);
    }

    /** 'Move' segment appender. */
    void append(std::string&& newSegment) {
        ASSERT(!newSegment.empty(), "provided segment is empty");
        ASSERT(newSegment.find_first_of('.') == std::string::npos, "segment may not contain a '.': " << newSegment);
        this->segments.push_back(std::move(newSegment));
        if (! this->ns.empty())
            this->ns.append(".");
        this->ns.append(this->segments.back());
    }


    /** Removes the last segment from this identifier and returns it. */
    const std::string pop();


    inline const TxIdentifier parent() const {
        ASSERT(this->is_qualified(), "identifier '" << this << "' is unqualified and has no parent namespace");
        return TxIdentifier(*this, 0, this->segments.size()-1);
    }

    /** Gets the local name of this identifier, its last name segment. */
    inline const std::string& name() const {
        return this->segments.back();
    }

    /** Returns true if this identifier has 2 or more name segments, false otherwise. */
    inline bool is_qualified() const {
        return this->segments.size() > 1;
    }

    /** Returns true if this identifier has exactly one name segment, false otherwise. */
    inline bool is_plain() const {
        return this->segments.size() == 1;
    }

    inline bool is_empty() const {
        return this->segments.size() == 0;
    }

    inline size_t segment_count() const {
        return this->segments.size();
    }

    inline const std::string& segment(int index) const {
        return this->segments.at(index);
    }

    inline std::vector<std::string>::const_iterator segments_cbegin() const { return this->segments.cbegin(); }
    inline std::vector<std::string>::const_iterator segments_cend()   const { return this->segments.cend(); }
    inline std::vector<std::string>::const_reverse_iterator segments_crbegin() const { return this->segments.crbegin(); }
    inline std::vector<std::string>::const_reverse_iterator segments_crend()   const { return this->segments.crend(); }


    inline bool begins_with(const TxIdentifier& other) const {
        if (other.segments.size() > this->segments.size())
            return false;
        for (size_t i = 0; i < other.segments.size(); i++)
            if (other.segments.at(i) != this->segments.at(i))
                return false;
        return true;
    }
    inline bool begins_with(const std::string& namespaceStr) const {
        return this->ns.compare(0, namespaceStr.length(), namespaceStr) == 0;
    }
    inline bool begins_with(const char* namespaceStr) const {
        return this->begins_with(std::string(namespaceStr));
    }


    inline bool operator==(const TxIdentifier& other) const {
        return this->to_string() == other.to_string();
    }

    inline bool operator!=(const TxIdentifier& other) const {
        return this->to_string() != other.to_string();
    }

    inline bool operator<(const TxIdentifier& other) const {
        return this->to_string() < other.to_string();
    }

    inline bool operator<=(const TxIdentifier& other) const {
        return this->to_string() <= other.to_string();
    }


    inline virtual std::string to_string() const {
        return this->ns;
    }
};


///** Concatenation operator. */
//inline TxIdentifier operator+(const TxIdentifier& lhs, const TxIdentifier& rhs) {
//    TxIdentifier ident(lhs);
//    ident.appendIdent(rhs);
//    return ident;
//}


namespace std {
    template <> struct hash<TxIdentifier> {
        std::size_t operator()(const TxIdentifier& k) const {
            using std::size_t;
            using std::hash;
            using std::string;
            // Compute individual hash values and combine them using XOR and bit shifting:
            return ( hash<string>()(k.to_string()) );
        }
    };
}
