import { Accordion, AccordionItem } from "@chakra-ui/accordion";
import { ExpandedIndex, usePrefersReducedMotion } from "@chakra-ui/react";
import React, { useContext } from "react";

import { NavigationContext } from "./Navigation";

export const NavCategory: React.FC = ({ children, ...props }) => {
  const reduceMotion = usePrefersReducedMotion();
  const { activeIndex, setActiveIndex } = useContext(NavigationContext);

  const onChange = (index: ExpandedIndex) => {
    let expandedIndex: number[];
    if (typeof index === 'number') {
      expandedIndex = [index];
    } else {
      expandedIndex = index;
    }

    setActiveIndex(expandedIndex);
  };

  return (
    <Accordion
      defaultIndex={activeIndex}
      allowMultiple
      {...props}
      reduceMotion={reduceMotion}
      onChange={onChange}
    >
      {React.Children.toArray(children).map((child, index) => {
        return (
          <AccordionItem
            key={index}
            css={{
              listStyleType: "none",
              marginBottom: 32,
              border: "none",
            }}
          >
            {child}
          </AccordionItem>
        );
      })}
    </Accordion>
  );
};
