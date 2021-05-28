import { AccordionPanel } from "@chakra-ui/accordion";
import { List } from "@chakra-ui/react";

export const NavList: React.FC = ({ children }) => {
  return (
    <AccordionPanel>
      <List>{children}</List>
    </AccordionPanel>
  );
};
