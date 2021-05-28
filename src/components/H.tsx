import { Heading } from "@chakra-ui/react";
import useScreenWiderThanBreakpoint from "../hooks/useScreenWiderThanBreakpoint";

export const H: React.FC<Record<string, any>> = ({ as, ...props }) => {
  const isWide = useScreenWiderThanBreakpoint(720);

  if (as === "1")
    return (
      <Heading
        as="h1"
        mb="12"
        mt="4"
        fontSize={isWide ? "4xl" : "2xl"}
        css={{ paddingBottom: 24, borderBottom: "2px solid lightgrey" }}
        {...props}
      />
    );
  if (as === "2") {
    return (
      <Heading
        as="h2"
        fontSize={isWide ? "2xl" : "xl"}
        mb="4"
        mt="4"
        {...props}
      />
    );
  }
  if (as === "3") {
    return (
      <Heading
        as="h3"
        fontSize={isWide ? "xl" : "md"}
        mb="4"
        mt="2"
        {...props}
      />
    );
  }

  return <Heading {...props} />;
};
