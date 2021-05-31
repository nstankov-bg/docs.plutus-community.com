import { H } from "../H";
import { AccordionButton, AccordionIcon, useColorModeValue } from "@chakra-ui/react";

export const NavHeader: React.FC = ({ children, ...props }) => {
  const accentColor = useColorModeValue("gray.300", "gray.600");
  return (
    <H as="2" fontSize="md" m="0" p="0" {...props}>
      <AccordionButton
        css={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          textAlign: "left",
        }}
        borderBottomWidth="thin"
        borderBottomStyle="solid"
        borderBottomColor={accentColor}
        _hover={{
          backgroundColor: accentColor,
        }}
      >
        {children}
        <AccordionIcon />
      </AccordionButton>
    </H>
  );
};
