#include <iostream>

#include "identifier.hpp"


const std::string TxIdentifier::pop() {
    const std::string last_segment(this->segments.back());
    this->segments.pop_back();
    if (this->segments.size())
        this->ns.erase(this->ns.length()-last_segment.length()-1);  // (including period)
    else
        this->ns.clear();
    return last_segment;
}

//int TxIdentifier::parseC = 0;
//int TxIdentifier::appendCopCopC = 0;
//int TxIdentifier::appendCopMovC = 0;
//int TxIdentifier::appendMovMovC = 0;
//int TxIdentifier::subidentC = 0;
//int TxIdentifier::copyC = 0;
//int TxIdentifier::moveC = 0;
//
//void TxIdentifier::printStats() {
//    std::cout << "parseC: " << TxIdentifier::parseC << std::endl;
//    std::cout << "appendCopCopC: " << TxIdentifier::appendCopCopC << std::endl;
//    std::cout << "appendCopMovC: " << TxIdentifier::appendCopMovC << std::endl;
//    std::cout << "appendMovMovC: " << TxIdentifier::appendMovMovC << std::endl;
//    std::cout << "parentC: " << TxIdentifier::subidentC << std::endl;
//    std::cout << "copyC: " << TxIdentifier::copyC << std::endl;
//    std::cout << "moveC: " << TxIdentifier::moveC << std::endl;
//}
